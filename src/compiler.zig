const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;

const errout = std.io.getStdErr().writer();

const CompileError = error{CompileError,TooManyConstants};

pub fn compile(source: []const u8, chunk: *Chunk) CompileError!void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);

    try parser.advance();
    try parser.expression();

    if (scanner.nextToken()) |_| {
        parser.errAtCurrent("Expect end of expression.");
        return CompileError.CompileError;
    }

    parser.endCompiler();
}

const Precedence = enum {
    precNone,
    precAssignment, // =
    precOr, // or
    precAnd, // and
    precEquality, // == !=
    precComparison, // < > <= >=
    precTerm, // + -
    precFactor, // * /
    precUnary, // ! -
    precCall, // . ()
    precPrimary,
};

const ParseFn = *const fn(*Parser) CompileError!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    pub fn init(prefix: ?ParseFn, infix: ?ParseFn, precedence: Precedence) ParseRule {
        return .{
            .prefix = prefix,
            .infix = infix,
            .precedence = precedence,
        };
    }
};

fn getRule(token_type: TokenType) ParseRule {
    return switch (token_type) {
        .LeftParen => ParseRule.init(Parser.grouping, null, .precNone),
        .RightParen => ParseRule.init(null, null, .precNone),
        .LeftBrace => ParseRule.init(null, null, .precNone),
        .RightBrace => ParseRule.init(null, null, .precNone),
        .Comma => ParseRule.init(null, null, .precNone),
        .Dot => ParseRule.init(null, null, .precNone),
        .Minus => ParseRule.init(Parser.unary, Parser.binary, .precTerm),
        .Plus => ParseRule.init(null, Parser.binary, .precTerm),
        .Semicolon => ParseRule.init(null, null, .precNone),
        .Slash => ParseRule.init(null, Parser.binary, .precFactor),
        .Star => ParseRule.init(null, Parser.binary, .precFactor),
        .Bang => ParseRule.init(null, null, .precNone),
        .BangEqual => ParseRule.init(null, null, .precNone),
        .Equal => ParseRule.init(null, null, .precNone),
        .EqualEqual => ParseRule.init(null, null, .precNone),
        .Greater => ParseRule.init(null, null, .precNone),
        .GreaterEqual => ParseRule.init(null, null, .precNone),
        .Less => ParseRule.init(null, null, .precNone),
        .LessEqual => ParseRule.init(null, null, .precNone),
        .Identifier => ParseRule.init(null, null, .precNone),
        .String => ParseRule.init(null, null, .precNone),
        .Number => ParseRule.init(Parser.number, null, .precNone),
        .And => ParseRule.init(null, null, .precNone),
        .Class => ParseRule.init(null, null, .precNone),
        .Else => ParseRule.init(null, null, .precNone),
        .False => ParseRule.init(null, null, .precNone),
        .For => ParseRule.init(null, null, .precNone),
        .Fun => ParseRule.init(null, null, .precNone),
        .If => ParseRule.init(null, null, .precNone),
        .Nil => ParseRule.init(null, null, .precNone),
        .Or => ParseRule.init(null, null, .precNone),
        .Print => ParseRule.init(null, null, .precNone),
        .Return => ParseRule.init(null, null, .precNone),
        .Super => ParseRule.init(null, null, .precNone),
        .This => ParseRule.init(null, null, .precNone),
        .True => ParseRule.init(null, null, .precNone),
        .Var => ParseRule.init(null, null, .precNone),
        .While => ParseRule.init(null, null, .precNone),
        .Error => ParseRule.init(null, null, .precNone),
        .Eof => ParseRule.init(null, null, .precNone),
    };
}

const Parser = struct {
    const Self = @This();

    current: Token = undefined,
    previous: Token = undefined,
    scanner: *Scanner,
    chunk: *Chunk,
    hadError: bool = false,
    panicMode: bool = false,

    pub fn init(scanner: *Scanner, chunk: *Chunk) Self {
        return Self{ .scanner = scanner, .chunk = chunk };
    }

    pub fn advance(self: *Self) CompileError!void {
        self.previous = self.current;

        while (self.scanner.nextToken()) |token| {
            self.current = token;
            if (token.token_type != TokenType.Error) break;

            self.errAtCurrent(self.current.lexeme);
            return CompileError.CompileError;
        }
    }

    pub fn consume(self: *Self, token_type: TokenType, message: []const u8) CompileError!void {
        if (self.current.token_type == token_type) {
            try self.advance();
            return;
        }

        self.errAtCurrent(message);
        return CompileError.CompileError;
    }

    pub fn expression(self: *Self) !void {
        try self.parsePrecendece(.precAssignment);
    }

    fn number(self: *Self) !void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(value);
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn unary(self: *Self) !void {
        const operatorType = self.previous.token_type;
        try self.parsePrecendece(.precUnary);
        switch (operatorType) {
            .Minus => self.emitByte(OpCode.op_negate.toU8()),
            else => unreachable,
        }
    }

    fn binary(self: *Self) !void {
        const operatorType = self.previous.token_type;
        const rule = getRule(operatorType);
        try self.parsePrecendece(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

        switch (operatorType) {
            .Plus => self.emitByte(OpCode.op_add.toU8()),
            .Minus => self.emitByte(OpCode.op_subtract.toU8()),
            .Star => self.emitByte(OpCode.op_multiply.toU8()),
            .Slash => self.emitByte(OpCode.op_divide.toU8()),
            else => unreachable,
        }
    }

    fn parsePrecendece(self: *Self, precedence: Precedence) CompileError!void {
        try self.advance();
        const prefixRule = getRule(self.previous.token_type).prefix orelse {
            self.err("Expect expression.");
            return CompileError.CompileError;
        };

        try prefixRule(self);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.current.token_type).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.token_type);
            
            const infixRule = rule.infix orelse {
                self.err("Unreachable????");
                return CompileError.CompileError;
            };

            try infixRule(self);
        }
    }

    fn errAtCurrent(self: *Self, message: []const u8) void {
        self.errAt(self.current, message);
    }

    fn err(self: *Self, message: []const u8) void {
        self.errAt(self.previous, message);
    }

    fn errAt(self: *Self, token: Token, message: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;

        if (self.hadError) return;
        self.hadError = true;

        errout.print("[line {d}] Error", .{token.line}) catch unreachable;

        switch (token.token_type) {
            .Eof => errout.writeAll(" at end") catch unreachable,
            .Error => {},
            else => errout.print(" at '{s}'", .{token.lexeme}) catch unreachable,
        }

        errout.print(": {s}\n", .{message}) catch unreachable;
    }

    fn emitConstant(self: *Self, value: Value) !void {
        const constIdx = try self.makeConstant(value);
        self.emitBytes(OpCode.op_constant.toU8(), constIdx);
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        const constant = self.chunk.addConstant(value) catch {
            self.err("Err adding constant.");
            return CompileError.CompileError;
        };
        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk.");
            return CompileError.TooManyConstants;
        }

        return @intCast(u8, constant);
    }

    pub fn emitByte(self: *Self, byte: u8) void {
        self.chunk.write(byte, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte", .{e});
            std.process.exit(1);
        };
    }

    pub fn emitBytes(self: *Self, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    pub fn endCompiler(self: *Self) void {
        self.emitReturn();        
    }

    fn emitReturn(self: *Self) void {
        self.emitByte(OpCode.op_return.toU8());
    }
};
