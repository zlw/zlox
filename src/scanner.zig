const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    line: usize,
};

pub const Scanner = struct {
    const Self = @This();

    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8) Self {
        return Self{ .source = source };
    }

    pub fn nextToken(self: *Self) ?Token {
        self.skipWhiteSpace();
        if (self.isAtEnd()) return null;
        self.start = self.current;

        const c = self.advance();
        if (isAlpha(c)) return self.handleIdentifier();
        if (isDigit(c)) return self.handleNumber();

        return switch (c) {
            '(' => self.makeToken(.LeftParen),
            ')' => self.makeToken(TokenType.RightParen),
            '{' => self.makeToken(TokenType.LeftBrace),
            '}' => self.makeToken(TokenType.RightBrace),
            ';' => self.makeToken(TokenType.Semicolon),
            ',' => self.makeToken(TokenType.Comma),
            '.' => self.makeToken(TokenType.Dot),
            '-' => self.makeToken(TokenType.Minus),
            '+' => self.makeToken(TokenType.Plus),
            '/' => self.makeToken(TokenType.Slash),
            '*' => self.makeToken(TokenType.Star),
            '!' => if (self.match('=')) self.makeToken(TokenType.BangEqual) else self.makeToken(TokenType.Bang),
            '=' => if (self.match('=')) self.makeToken(TokenType.EqualEqual) else self.makeToken(TokenType.Equal),
            '<' => if (self.match('=')) self.makeToken(TokenType.LessEqual) else self.makeToken(TokenType.Less),
            '>' => if (self.match('=')) self.makeToken(TokenType.GreaterEqual) else self.makeToken(TokenType.Greater),
            '"' => self.handleString(),
            else => return self.makeError("Unexpected character."),
        };
    }

    fn handleIdentifier(self: *Self) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            self.current += 1;
        }

        return self.makeToken(self.identifierType());
    }

    fn handleNumber(self: *Self) Token {
        while (isDigit(self.peek())) {
            self.current += 1;
        }

        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // consume the dot
            self.current += 1;

            while (isDigit(self.peek())) {
                self.current += 1;
            }
        }

        return self.makeToken(TokenType.Number);
    }

    fn handleString(self: *Self) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            self.current += 1;
        }

        if (self.isAtEnd()) return self.makeError("Unterminated string.");

        // For the closing quote
        self.current += 1;
        return self.makeToken(TokenType.String);
    }

    fn skipWhiteSpace(self: *Self) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => self.current += 1,
                '\n' => {
                    self.line += 1;
                    self.current += 1;
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.current += 1;
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    inline fn peek(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    inline fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current + 1];
    }

    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    fn isAtEnd(self: *Self) bool {
        return self.current >= self.source.len;
    }

    fn makeToken(self: *Self, tokenType: TokenType) Token {
        return Token{
            .token_type = tokenType,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn makeError(self: *Self, message: []const u8) Token {
        return Token{
            .token_type = TokenType.Error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn identifierType(self: *Scanner) TokenType {
        return switch (self.peek()) {
            'a' => self.checkKeyword(1, "nd", TokenType.And),
            'c' => self.checkKeyword(1, "lass", TokenType.Class),
            'e' => self.checkKeyword(1, "lse", TokenType.Else),
            'i' => self.checkKeyword(1, "f", TokenType.If),
            'n' => self.checkKeyword(1, "il", TokenType.Nil),
            'o' => self.checkKeyword(1, "r", TokenType.Or),
            'p' => self.checkKeyword(1, "rint", TokenType.Print),
            'r' => self.checkKeyword(1, "eturn", TokenType.Return),
            's' => self.checkKeyword(1, "uper", TokenType.Super),
            'v' => self.checkKeyword(1, "ar", TokenType.Var),
            'w' => self.checkKeyword(1, "hile", TokenType.While),
            'f' => {
                return switch (self.peekNext()) {
                    'a' => self.checkKeyword(2, "lse", TokenType.False),
                    'o' => self.checkKeyword(2, "r", TokenType.For),
                    'u' => self.checkKeyword(2, "n", TokenType.Fun),
                    else => TokenType.Identifier,
                };
            },
            't' => {
                return switch (self.peekNext()) {
                    'h' => self.checkKeyword(2, "is", TokenType.This),
                    'r' => self.checkKeyword(2, "ue", TokenType.True),
                    else => TokenType.Identifier,
                };
            },
            else => TokenType.Identifier,
        };
    }

    fn checkKeyword(self: *Scanner, offset: usize, str: []const u8, token_type: TokenType) TokenType {
        if (self.current != str.len + offset) return TokenType.Identifier;
        
        const sourceSlice = self.source[offset..self.current];
        std.debug.assert(sourceSlice.len == str.len);
        return if (std.mem.eql(u8, sourceSlice, str)) token_type else TokenType.Identifier;
    }
};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
}