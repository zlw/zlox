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

const debug = @import("./debug.zig");
const debug_rule_selection = debug.debug_rule_selection;
const debug_print_code = debug.debug_print_code;
const locals_max = std.math.maxInt(u8) + 1;

const CompileError = error{ CompileError, TooManyConstants };

pub fn compile(vm: *Vm, source: []const u8) CompileError!*Object.Function {
    var scanner = Scanner.init(source);
    var compiler = Compiler.init(vm, FunctionType.Script);
    var parser = Parser.init(vm, &scanner, &compiler);

    parser.advance();

    while (!parser.match(.Eof)) {
        parser.declaration();
    }
    parser.consume(.Eof, "Expect end of expression");
    const function = parser.endCompiler();

    return if (parser.hadError) CompileError.CompileError else function;
}

const Precedence = enum {
    None,
    Assignment, // =
    Or, // or
    And, // and
    Equality, // == !=
    Comparison, // < > <= >=
    Term, // + -
    Factor, // * /
    Unary, // ! -
    Call, // . ()
    Primary,
};

const ParseFn = *const fn (parser: *Parser, canAssign: bool) void;

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
        .LeftParen => ParseRule.init(Parser.grouping, Parser.call, Precedence.Call),
        .RightParen => ParseRule.init(null, null, Precedence.None),
        .LeftBrace => ParseRule.init(null, null, Precedence.None),
        .RightBrace => ParseRule.init(null, null, Precedence.None),
        .Comma => ParseRule.init(null, null, Precedence.None),
        .Dot => ParseRule.init(null, null, Precedence.None),
        .Minus => ParseRule.init(Parser.unary, Parser.binary, Precedence.Term),
        .Plus => ParseRule.init(null, Parser.binary, Precedence.Term),
        .Semicolon => ParseRule.init(null, null, Precedence.None),
        .Slash => ParseRule.init(null, Parser.binary, Precedence.Factor),
        .Star => ParseRule.init(null, Parser.binary, Precedence.Factor),
        .Bang => ParseRule.init(Parser.unary, null, Precedence.None),
        .BangEqual => ParseRule.init(null, Parser.binary, Precedence.Equality),
        .Equal => ParseRule.init(null, null, Precedence.None),
        .EqualEqual => ParseRule.init(null, Parser.binary, Precedence.Equality),
        .Greater => ParseRule.init(null, Parser.binary, Precedence.Comparison),
        .GreaterEqual => ParseRule.init(null, Parser.binary, Precedence.Comparison),
        .Less => ParseRule.init(null, Parser.binary, Precedence.Comparison),
        .LessEqual => ParseRule.init(null, Parser.binary, Precedence.Comparison),
        .Identifier => ParseRule.init(Parser.variable, null, Precedence.None),
        .String => ParseRule.init(Parser.string, null, Precedence.None),
        .Number => ParseRule.init(Parser.number, null, Precedence.None),
        .And => ParseRule.init(null, Parser.logical_and, Precedence.And),
        .Class => ParseRule.init(null, null, Precedence.None),
        .Else => ParseRule.init(null, null, Precedence.None),
        .False => ParseRule.init(Parser.literal, null, Precedence.None),
        .For => ParseRule.init(null, null, Precedence.None),
        .Fun => ParseRule.init(null, null, Precedence.None),
        .If => ParseRule.init(null, null, Precedence.None),
        .Nil => ParseRule.init(Parser.literal, null, Precedence.None),
        .Or => ParseRule.init(null, Parser.logical_or, Precedence.Or),
        .Print => ParseRule.init(null, null, Precedence.None),
        .Return => ParseRule.init(null, null, Precedence.None),
        .Super => ParseRule.init(null, null, Precedence.None),
        .This => ParseRule.init(null, null, Precedence.None),
        .True => ParseRule.init(Parser.literal, null, Precedence.None),
        .Var => ParseRule.init(null, null, Precedence.None),
        .While => ParseRule.init(null, null, Precedence.None),
        .Error => ParseRule.init(null, null, Precedence.None),
        .Eof => ParseRule.init(null, null, Precedence.None),
    };

    if (comptime debug_rule_selection) {
        std.debug.print("{}\n", .{rule});
    }

    return rule;
}

const FunctionType = enum { Function, Script };

const Compiler = struct {
    const Self = @This();

    enclosing: ?*Compiler = null,

    function: *Object.Function,
    functionType: FunctionType,

    locals: [locals_max]Local = undefined,
    localCount: u16 = 0,
    scopeDepth: u32 = 0,

    fn init(vm: *Vm, functionType: FunctionType) Self {
        var compiler = Self{ .function = Object.Function.create(vm), .functionType = functionType };

        var local = &compiler.locals[compiler.localCount];
        compiler.localCount += 1;
        local.depth = 0;
        local.name.lexeme = "";


        return compiler;
    }
};

const Local = struct {
    name: Token,
    depth: ?u32,
};

const Parser = struct {
    const Self = @This();

    vm: *Vm,
    current: Token = undefined,
    previous: Token = undefined,
    scanner: *Scanner,
    compiler: *Compiler,
    hadError: bool = false,
    panicMode: bool = false,

    pub fn init(vm: *Vm, scanner: *Scanner, compiler: *Compiler) Self {
        return Self{ .vm = vm, .scanner = scanner, .compiler = compiler };
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.compiler.function.chunk;
    }

    fn advance(self: *Self) void {
        self.previous = self.current;

        while (self.scanner.nextToken()) |token| {
            self.current = token;
            if (token.token_type != TokenType.Error) break;

            self.errAtCurrent(self.current.lexeme);
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) void {
        if (self.check(token_type)) {
            self.advance();
            return;
        }

        self.errAtCurrent(message);
    }

    fn expression(self: *Self) void {
        self.parsePrecedence(Precedence.Assignment);
    }

    fn match(self: *Self, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;

        self.advance();
        return true;
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.current.token_type == token_type;
    }

    fn declaration(self: *Self) void {
        if (self.match(TokenType.Fun)) {
            self.funDeclaration();
        } else if (self.match(TokenType.Var)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    fn synchronize(self: *Self) void {
        self.panicMode = false;

        while (!self.check(TokenType.Eof)) {
            if (self.previous.token_type == TokenType.Semicolon) return;

            switch (self.current.token_type) {
                .Class, .Fun, .Var, .For, .If, .While, .Print, .Return => return,
                else => self.advance(),
            }
        }
    }

    fn funDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect function name");
        self.markInitialized();
        self.compileFunction(FunctionType.Function);
        self.defineVariable(global);
    }

    fn compileFunction(self: *Self, functionType: FunctionType) void {
        var functionCompiler = Compiler.init(self.vm, functionType);
        functionCompiler.enclosing = self.compiler;
        self.compiler = &functionCompiler;

        // book does this in initCompiler which is our Compiler.init,
        // but I don't see a reason why if we're setting function.arity here, we can as well set it's name
        // that way we don't need to make Compiler depend on the Parser which would create bi-directional dependency
        functionCompiler.function.name = Object.String.copy(self.vm, self.previous.lexeme);

        self.beginScope();

        self.consume(TokenType.LeftParen, "Expect '(' after function name");
        if (!self.check(TokenType.RightParen)) {
            while (true) {
                self.compiler.function.arity += 1;

                if (self.compiler.function.arity > 255) {
                    self.errAtCurrent("Can't have more than 255 parameters");                    
                    return;
                }

                const parameter = self.parseVariable("Expect parameter name");
                self.defineVariable(parameter);

                if (!self.match(TokenType.Comma)) break;
            }
        }
        self.consume(TokenType.RightParen, "Expect ')' after parameters");
        self.consume(TokenType.LeftBrace, "Expect '{' before function body");

        self.block();

        const function = self.endCompiler();

        self.emitOpAndByte(OpCode.op_constant, self.makeConstant(Value.ObjectValue(&function.object)));
    }

    fn varDeclaration(self: *Self) void {
        const global = self.parseVariable("Expect variable name");

        if (self.match(TokenType.Equal)) {
            self.expression();
        } else {
            self.emitOp(OpCode.op_nil);
        }

        self.consume(TokenType.Semicolon, "Expect ';' after variable declaration");

        self.defineVariable(global);
    }

    fn statement(self: *Self) void {
        if (self.match(TokenType.Print)) {
            self.printStatement();
        } else if (self.match(TokenType.For)) {
            self.forStatement();
        } else if (self.match(TokenType.If)) {
            self.ifStatement();
        } else if (self.match(TokenType.Return)) {
            self.returnStatement();
        } else if (self.match(TokenType.While)) {
            self.whileStatement();
        } else if (self.match(TokenType.LeftBrace)) {
            self.beginScope();
            self.block();
            self.endScope();
        } else {
            self.expressionStatement();
        }
    }

    fn printStatement(self: *Self) void {
        self.expression();
        self.consume(TokenType.Semicolon, "Expected ';' after value");
        self.emitOp(OpCode.op_print);
    }

    fn forStatement(self: *Self) void {
        self.beginScope();
        self.consume(TokenType.LeftParen, "Expect '(' after 'for'");

        if (self.match(TokenType.Semicolon)) {
            // No initializer
        } else if (self.match(TokenType.Var)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loopStart = self.currentChunk().code.count;
        var exitJump: ?usize = null;

        if (!self.match(TokenType.Semicolon)) {
            self.expression();
            self.consume(TokenType.Semicolon, "Expect ';' after loop condition");

            exitJump = self.emitJump(OpCode.op_jump_if_false);
            self.emitOp(OpCode.op_pop);
        }

        if (!self.match(TokenType.RightParen)) {
            const bodyJump = self.emitJump(OpCode.op_jump);
            const incrementStart = self.currentChunk().code.count;

            self.expression();
            self.emitOp(OpCode.op_pop);
            self.consume(TokenType.RightParen, "Expect ')' after for clauses");

            self.emitLoop(loopStart);
            loopStart = incrementStart;
            self.patchJump(bodyJump);
        }

        self.statement();
        self.emitLoop(loopStart);

        if (exitJump) |jump| {
            self.patchJump(jump);
            self.emitOp(OpCode.op_pop);
        }

        self.endScope();
    }

    fn ifStatement(self: *Self) void {
        self.consume(TokenType.LeftParen, "Expect '(' after 'if'");
        self.expression();
        self.consume(TokenType.RightParen, "Expect ')' after condition");

        const thenJump = self.emitJump(OpCode.op_jump_if_false);
        self.emitOp(OpCode.op_pop);
        self.statement();

        const elseJump = self.emitJump(OpCode.op_jump);

        self.patchJump(thenJump);
        self.emitOp(OpCode.op_pop);

        if (self.match(TokenType.Else)) self.statement();

        self.patchJump(elseJump);
    }

    fn returnStatement(self: *Self) void {
        if (self.compiler.functionType == FunctionType.Script) self.err("Can't return from top-level code");

        if (self.match(TokenType.Semicolon)) {
            self.emitReturn();
        } else {
            self.expression();
            self.consume(TokenType.Semicolon, "Expect ';' after return value");
            self.emitOp(OpCode.op_return);
        }
    }

    fn whileStatement(self: *Self) void {
        const loopStart = self.currentChunk().code.count;
        self.consume(TokenType.LeftParen, "Expect '(' after 'while'");
        self.expression();
        self.consume(TokenType.RightParen, "Expect ')' after condition");

        const exitJump = self.emitJump(OpCode.op_jump_if_false);
        self.emitOp(OpCode.op_pop);
        self.statement();

        self.emitLoop(loopStart);

        self.patchJump(exitJump);
        self.emitOp(OpCode.op_pop);
    }

    fn beginScope(self: *Self) void {
        self.compiler.scopeDepth += 1;
    }

    fn block(self: *Self) void {
        while (!self.check(TokenType.RightBrace) and !self.check(TokenType.Eof)) {
            self.declaration();
        }

        self.consume(TokenType.RightBrace, "Expect '}' after block");
    }

    fn endScope(self: *Self) void {
        self.compiler.scopeDepth -= 1;

        while (self.compiler.localCount > 0 and self.compiler.locals[self.compiler.localCount - 1].depth.? > self.compiler.scopeDepth) {
            self.emitOp(OpCode.op_pop);
            self.compiler.localCount -= 1;
        }
    }

    fn expressionStatement(self: *Self) void {
        self.expression();
        self.consume(TokenType.Semicolon, "Expect ';' after expression");
        self.emitOp(OpCode.op_pop);
    }

    fn number(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const value = std.fmt.parseFloat(f64, self.previous.lexeme) catch unreachable;
        self.emitConstant(Value.NumberValue(value));
    }

    fn string(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const lexeme = self.previous.lexeme;
        const value = Object.String.copy(self.vm, lexeme[1 .. lexeme.len - 1]);

        self.emitConstant(Value.ObjectValue(&value.object));
    }

    fn variable(self: *Self, canAssign: bool) void {
        self.namedVariable(&self.previous, canAssign);
    }

    fn namedVariable(self: *Self, name: *Token, canAssign: bool) void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;

        var arg = self.resolveLocal(name);

        if (arg) |_| {
            getOp = OpCode.op_get_local;
            setOp = OpCode.op_set_local;
        } else {
            arg = self.identifierConstant(name);

            getOp = OpCode.op_get_global;
            setOp = OpCode.op_set_global;
        }

        if (canAssign and self.match(TokenType.Equal)) {
            self.expression();
            self.emitOpAndByte(setOp, arg.?);
        } else {
            self.emitOpAndByte(getOp, arg.?);
        }
    }

    fn resolveLocal(self: *Self, name: *Token) ?u8 {
        var i: usize = self.compiler.localCount;
        while (i > 0) {
            i -= 1;
            const local = &self.compiler.locals[i];
            if (self.identifiersEqual(name, &local.name)) {
                if (local.depth == null) {
                    self.err("Can't read local variable in its own initializer");
                }
                return @as(u8, @intCast(i));
            }
        }

        return null;
    }

    fn grouping(self: *Self, canAssign: bool) void {
        _ = canAssign;
        self.expression();
        self.consume(.RightParen, "Expect ')' after expression");
    }

    fn unary(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const operatorType = self.previous.token_type;
        self.parsePrecedence(Precedence.Unary);
        switch (operatorType) {
            .Bang => self.emitOp(OpCode.op_not),
            .Minus => self.emitOp(OpCode.op_negate),
            else => unreachable,
        }
    }

    fn binary(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const operatorType = self.previous.token_type;
        const rule = getRule(operatorType);
        self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

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

    fn call(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const argCount = self.argumentList();
        self.emitOpAndByte(OpCode.op_call, argCount);
    }

    fn literal(self: *Self, canAssign: bool) void {
        _ = canAssign;
        switch (self.previous.token_type) {
            .False => self.emitOp(OpCode.op_false),
            .True => self.emitOp(OpCode.op_true),
            .Nil => self.emitOp(OpCode.op_nil),
            else => unreachable,
        }
    }

    fn logical_and(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const endJump = self.emitJump(OpCode.op_jump_if_false);

        self.emitOp(OpCode.op_pop);
        self.parsePrecedence(Precedence.And);

        self.patchJump(endJump);
    }

    fn logical_or(self: *Self, canAssign: bool) void {
        _ = canAssign;
        const elseJump = self.emitJump(OpCode.op_jump_if_false);
        const endJump = self.emitJump(OpCode.op_jump);

        self.patchJump(elseJump);
        self.emitOp(OpCode.op_pop);

        self.parsePrecedence(Precedence.Or);

        self.patchJump(endJump);
    }

    fn parsePrecedence(self: *Self, precedence: Precedence) void {
        self.advance();
        const prefixRule = getRule(self.previous.token_type).prefix orelse {
            self.err("Expect expression");
            return;
        };

        const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
        prefixRule(self, canAssign);

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.token_type).precedence)) {
            self.advance();
            const infixRule = getRule(self.previous.token_type).infix orelse {
                self.err("Expect expression");
                return;
            };
            
            infixRule(self, canAssign);
        }

        if (canAssign and self.match(TokenType.Equal)) {
            self.err("Invalid assignment target");
        }
    }

    fn identifierConstant(self: *Self, name: *Token) u8 {
        const identifier = Object.String.copy(self.vm, name.lexeme);
        return self.makeConstant(Value.ObjectValue(&identifier.object));
    }

    fn parseVariable(self: *Self, message: []const u8) u8 {
        self.consume(TokenType.Identifier, message);

        self.declareVariable();
        if (self.compiler.scopeDepth > 0) return 0;

        return self.identifierConstant(&self.previous);
    }

    fn defineVariable(self: *Self, global: u8) void {
        if (self.compiler.scopeDepth > 0) {
            self.markInitialized();
            return;
        }

        self.emitOpAndByte(OpCode.op_define_global, global);
    }

    fn argumentList(self: *Self) u8 {
        var argCount: u8 = 0;

        if (!self.check(TokenType.RightParen)) {
            while (true) {
                self.expression();

                if (argCount >= 255) {
                    self.err("Can't have more than 255 arguments");
                    break;
                }
                argCount += 1;

                if (!self.match(TokenType.Comma)) break;
            }
        }

        self.consume(TokenType.RightParen, "Expect ')' after arguments.");

        return argCount;
    }

    fn markInitialized(self: *Self) void {
        if (self.compiler.scopeDepth == 0) return;
        self.compiler.locals[self.compiler.localCount - 1].depth = self.compiler.scopeDepth;
    }

    fn declareVariable(self: *Self) void {
        if (self.compiler.scopeDepth == 0) return;

        const name = &self.previous;

        var i: usize = self.compiler.localCount;
        while (i > 0) {
            i -= 1;
            const local = &self.compiler.locals[i];

            if (local.depth != null and local.depth.? < self.compiler.scopeDepth) break;

            if (self.identifiersEqual(name, &local.name)) {
                self.err("Already a variable with this name in this scope");
            }
        }

        self.addLocal(name);
    }

    fn identifiersEqual(self: *Self, a: *Token, b: *Token) bool {
        _ = self;
        return std.mem.eql(u8, a.lexeme, b.lexeme);
    }

    fn addLocal(self: *Self, name: *Token) void {
        if (self.compiler.localCount >= locals_max) {
            self.err("Too many local variables in function");
            return;
        }

        self.compiler.locals[self.compiler.localCount] = Local{ .name = name.*, .depth = null };
        self.compiler.localCount += 1;
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

        errout.print("[line {d}] Error", .{token.line}) catch unreachable;

        switch (token.token_type) {
            .Eof => errout.writeAll(" at end") catch unreachable,
            .Error => {},
            else => errout.print(" at '{s}'", .{token.lexeme}) catch unreachable,
        }

        errout.print(": {s}.\n", .{message}) catch unreachable;

        self.hadError = true;
    }

    fn emitConstant(self: *Self, value: Value) void {
        const constIdx = self.makeConstant(value);
        self.emitOpAndByte(OpCode.op_constant, constIdx);
    }

    fn makeConstant(self: *Self, value: Value) u8 {
        const constant = self.currentChunk().addConstant(value) catch {
            self.err("Err adding constant");
        };

        if (constant > std.math.maxInt(u8)) {
            self.err("Too many constants in a chunk");
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

    fn emitJump(self: *Self, code: OpCode) usize {
        self.emitOp(code);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().code.count - 2;
    }

    fn patchJump(self: *Self, offset: usize) void {
        const jump = self.currentChunk().code.count - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            self.err("Too much code to jump over");
        }

        self.currentChunk().code.items[offset] = @as(u8, @truncate(jump >> 8)) & 0xff;
        self.currentChunk().code.items[offset + 1] = @as(u8, @truncate(jump)) & 0xff;
    }

    fn emitLoop(self: *Self, loopStart: usize) void {
        self.emitOp(OpCode.op_loop);

        const offset = self.currentChunk().code.count - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            self.err("Loop body too large");
        }

        self.emitByte(@as(u8, @truncate(offset >> 8)) & 0xff);
        self.emitByte(@as(u8, @truncate(offset)) & 0xff);
    }

    fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.previous.line) catch |e| {
            std.log.err("Error {any} trying to emit byte", .{e});
            std.process.exit(1);
        };
    }

    fn endCompiler(self: *Self) *Object.Function {
        self.emitReturn();
        const function = self.compiler.function;

        if (comptime debug_print_code) {
            if (!self.hadError) {
                const name = if (function.name) |n| n.chars else "<script>";
                debug.disassembleChunk(self.currentChunk(), name);
            }
        }

        if (self.compiler.enclosing) |enclosing| self.compiler = enclosing;
        return function;
    }

    fn emitReturn(self: *Self) void {
        self.emitOp(OpCode.op_nil);
        self.emitOp(OpCode.op_return);

    }
};
