const std = @import("std");
const Scanner = @import("./scanner.zig").Scanner;
const Token = @import("./scanner.zig").Token;
const TokenType = @import("./scanner.zig").TokenType;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig").Object;
const Vm = @import("./vm.zig").Vm;

const errout = std.io.getStdErr().writer();

const debug_rule_selection = @import("./debug.zig").debug_rule_selection;

const CompileError = error{ CompileError, TooManyConstants };

pub fn compile(vm: *Vm, source: []const u8, chunk: *Chunk) CompileError!void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(vm, &scanner, chunk);

    try parser.advance();

    while (!try parser.match(.Eof)) {
        try parser.declaration();
    }
    try parser.consume(.Eof, "Expect end of expression.");
    parser.endCompiler();

    if (parser.hadError) return CompileError.CompileError;
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

const ParseFn = *const fn (*Parser) CompileError!void;

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
    if (comptime debug_rule_selection) {
        std.debug.print("{}\n", .{token_type});
    }

    const rule = switch (token_type) {
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
        .Bang => ParseRule.init(Parser.unary, null, .precNone),
        .BangEqual => ParseRule.init(null, Parser.binary, .precEquality),
        .Equal => ParseRule.init(null, null, .precNone),
        .EqualEqual => ParseRule.init(null, Parser.binary, .precEquality),
        .Greater => ParseRule.init(null, Parser.binary, .precComparison),
        .GreaterEqual => ParseRule.init(null, Parser.binary, .precComparison),
        .Less => ParseRule.init(null, Parser.binary, .precComparison),
        .LessEqual => ParseRule.init(null, Parser.binary, .precComparison),
        .Identifier => ParseRule.init(Parser.variable, null, .precNone),
        .String => ParseRule.init(Parser.string, null, .precNone),
        .Number => ParseRule.init(Parser.number, null, .precNone),
        .And => ParseRule.init(null, null, .precNone),
        .Class => ParseRule.init(null, null, .precNone),
        .Else => ParseRule.init(null, null, .precNone),
        .False => ParseRule.init(Parser.literal, null, .precNone),
        .For => ParseRule.init(null, null, .precNone),
        .Fun => ParseRule.init(null, null, .precNone),
        .If => ParseRule.init(null, null, .precNone),
        .Nil => ParseRule.init(Parser.literal, null, .precNone),
        .Or => ParseRule.init(null, null, .precNone),
        .Print => ParseRule.init(null, null, .precNone),
        .Return => ParseRule.init(null, null, .precNone),
        .Super => ParseRule.init(null, null, .precNone),
        .This => ParseRule.init(null, null, .precNone),
        .True => ParseRule.init(Parser.literal, null, .precNone),
        .Var => ParseRule.init(null, null, .precNone),
        .While => ParseRule.init(null, null, .precNone),
        .Error => ParseRule.init(null, null, .precNone),
        .Eof => ParseRule.init(null, null, .precNone),
    };

    if (comptime debug_rule_selection) {
        std.debug.print("{}\n", .{rule});
    }

    return rule;
}

const Parser = struct {
    const Self = @This();

    vm: *Vm,
    current: Token = undefined,
    previous: Token = undefined,
    scanner: *Scanner,
    chunk: *Chunk,
    hadError: bool = false,
    panicMode: bool = false,

    pub fn init(vm: *Vm, scanner: *Scanner, chunk: *Chunk) Self {
        return Self{ .vm = vm, .scanner = scanner, .chunk = chunk };
    }

    fn advance(self: *Self) CompileError!void {
        self.previous = self.current;

        while (self.scanner.nextToken()) |token| {
            self.current = token;
            if (token.token_type != TokenType.Error) break;

            self.errAtCurrent(self.current.lexeme);
            return CompileError.CompileError;
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) CompileError!void {
        if (self.check(token_type)) {
            try self.advance();
            return;
        }

        self.errAtCurrent(message);
        return CompileError.CompileError;
    }

    fn expression(self: *Self) !void {
        try self.parsePrecendece(.precAssignment);
    }

    fn match(self: *Self, token_type: TokenType) CompileError!bool {
        if (!self.check(token_type)) return false;

        try self.advance();
        return true;
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.current.token_type == token_type;
    }

    fn declaration(self: *Self) CompileError!void {
        if (try self.match(TokenType.Var)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
        
        if (self.panicMode) try self.synchronize();
    }

    fn synchronize(self: *Self) CompileError!void {
        self.panicMode = false;

        while (!self.check(TokenType.Eof)) {
            if (self.previous.token_type == TokenType.Semicolon) return;

            switch (self.current.token_type) {
                .Class, .Fun, .Var, .For, .If, 
                .While, .Print, .Return => return,
                else => try self.advance(),
            }
        }
    }

    fn varDeclaration(self: *Self) CompileError!void {
        const global = try self.parseVariable("Expect variable name");

        if (try self.match(TokenType.Equal)) {
            try self.expression();
        } else {
            self.emitOp(OpCode.op_nil);
        }

        try self.consume(TokenType.Semicolon, "Expect ';' after variable declaration");

        self.defineVariable(global);
    }

    fn statement(self: *Self) CompileError!void {
        if (try self.match(TokenType.Print)) { 
            try self.printStatement();
        } else {
            try self.expressionStatement();
        }
    }

    fn printStatement(self: *Self) CompileError!void {
        try self.expression();
        try self.consume(TokenType.Semicolon, "Expected ';' after value");
        self.emitOp(OpCode.op_print);
    }

    fn expressionStatement(self: *Self) CompileError!void {
        try self.expression();
        try self.consume(TokenType.Semicolon, "Expect ';' after expression.");
        self.emitOp(OpCode.op_pop);
    }

    fn number(self: *Self) !void {
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        try self.emitConstant(Value.NumberValue(value));
    }

    fn string(self: *Self) !void {
        const lexeme = self.previous.lexeme;
        const value = Object.String.copy(self.vm, lexeme[1..lexeme.len-1]);

        try self.emitConstant(Value.ObjectValue(&value.object));
    }

    fn variable(self: *Self) !void {
        try self.namedVariable(&self.previous);
    }

    fn namedVariable(self: *Self, name: *Token) !void {
        const arg = try self.identifierConstant(name);
        self.emitOpAndByte(OpCode.op_get_global, arg);
    }

    fn grouping(self: *Self) !void {
        try self.expression();
        try self.consume(.RightParen, "Expect ')' after expression.");
    }

    fn unary(self: *Self) !void {
        const operatorType = self.previous.token_type;
        try self.parsePrecendece(.precUnary);
        switch (operatorType) {
            .Bang => self.emitOp(OpCode.op_not),
            .Minus => self.emitOp(OpCode.op_negate),
            else => unreachable,
        }
    }

    fn binary(self: *Self) !void {
        const operatorType = self.previous.token_type;
        const rule = getRule(operatorType);
        try self.parsePrecendece(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (operatorType) {
            .BangEqual => self.emitOps(OpCode.op_equal, OpCode.op_not),
            .EqualEqual => self.emitOp(OpCode.op_equal),
            .Greater => self.emitOp(OpCode.op_greater),
            .GreaterEqual => self.emitOps(OpCode.op_less, OpCode.op_not),
            .Less => self.emitOp(OpCode.op_less),
            .LessEqual => self.emitOps(OpCode.op_greater, OpCode.op_not),
            .Plus => self.emitOp(OpCode.op_add),
            .Minus => self.emitOp(OpCode.op_subtract),
            .Star => self.emitOp(OpCode.op_multiply),
            .Slash => self.emitOp(OpCode.op_divide),
            else => unreachable,
        }
    }

    fn literal(self: *Self) !void {
        switch (self.previous.token_type) {
            .False => self.emitOp(OpCode.op_false),
            .True => self.emitOp(OpCode.op_true),
            .Nil => self.emitOp(OpCode.op_nil),
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

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.token_type).precedence)) {
            try self.advance();
            const rule = getRule(self.previous.token_type);

            const infixRule = rule.infix orelse {
                self.err("Unreachable????");
                return CompileError.CompileError;
            };

            try infixRule(self);
        }
    }

    fn identifierConstant(self: *Self, name: *Token) !u8 {
        const identifier = Object.String.copy(self.vm, name.lexeme);
        return self.makeConstant(Value.ObjectValue(&identifier.object));
    }

    fn parseVariable(self: *Self, message: []const u8) !u8 {
        try self.consume(TokenType.Identifier, message);
        return self.identifierConstant(&self.previous);
    }

    fn defineVariable(self: *Self, global: u8) void {
        self.emitOpAndByte(OpCode.op_define_global, global);
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
        self.emitOpAndByte(OpCode.op_constant, constIdx);
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

        return @as(u8, @intCast(constant));
    }

    fn emitOp(self: *Self, code: OpCode) void {
        self.emitByte(code.toU8());
    }

    fn emitOps(self: *Self, code1: OpCode, code2: OpCode) void {
        self.emitOp(code1);
        self.emitOp(code2);
    }

    fn emitOpAndByte(self: *Self, code: OpCode, byte: u8) void {
        self.emitOp(code);
        self.emitByte(byte);
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.chunk.write(byte, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte", .{e});
            std.process.exit(1);
        };
    }

    fn endCompiler(self: *Self) void {
        self.emitOp(OpCode.op_return);
    }
};
