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
    type: TokenType,
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
            .type = tokenType,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn makeError(self: *Self, message: []const u8) Token {
        return Token{
            .type = TokenType.Error,
            .lexeme = message,
            .line = self.line,
        };
    }

    fn identifierType(self: *Self) TokenType {
        return switch (self.source[self.start]) {
            'a' => self.checkKeyword("and", TokenType.And),
            'c' => self.checkKeyword("class", TokenType.Class),
            'e' => self.checkKeyword("else", TokenType.Else),
            'f' => switch (self.source[self.start + 1]) {
                'a' => self.checkKeyword("false", TokenType.False),
                'o' => self.checkKeyword("for", TokenType.For),
                'u' => self.checkKeyword("fun", TokenType.Fun),
                else => TokenType.Identifier,
            },
            'i' => self.checkKeyword("if", TokenType.If),
            'n' => self.checkKeyword("nil", TokenType.Nil),
            'o' => self.checkKeyword("or", TokenType.Or),
            'p' => self.checkKeyword("print", TokenType.Print),
            'r' => self.checkKeyword("return", TokenType.Return),
            's' => self.checkKeyword("super", TokenType.Super),
            't' => switch (self.source[self.start + 1]) {
                'h' => self.checkKeyword("this", TokenType.This),
                'r' => self.checkKeyword("true", TokenType.True),
                else => TokenType.Identifier,
            },
            'v' => self.checkKeyword("var", TokenType.Var),
            'w' => self.checkKeyword("while", TokenType.While),
            else => return TokenType.Identifier,
        };
    }

    fn checkKeyword(self: *Self, keyword: []const u8, ty: TokenType) TokenType {
        if (std.mem.eql(u8, keyword, self.source[self.start..self.current])) {
            return ty;
        }

        return TokenType.Identifier;
    }
};

fn isDigit(char: u8) bool {
    return char >= '0' and char <= '9';
}

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
}
