const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("./object.zig").Object;
const String = Object.String;
const debug = @import("./debug.zig");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

const Table = @import("./table.zig").Table;

const compile = @import("./compiler.zig").compile;

const native = @import("./native.zig");

const GarbageCollector = @import("./memory.zig").GarbageCollector;
const Parser = @import("./compiler.zig").Parser;

const debug_trace_execution = debug.debug_trace_execution;
const debug_stack_execution = debug.debug_stack_execution;
const debug_garbage_collection = debug.debug_garbage_collection;

const frames_max = 64;
const stack_max = frames_max * std.math.maxInt(u8);

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const BinaryOp = enum { add, sub, mul, div };
const CompOp = enum { gt, lt };

const CallFrame = struct {
    closure: *Object.Closure,
    ip: [*]u8 = 0,
    slots: [*]Value,
};

pub const Vm = struct {
    const Self = @This();

    frames: [frames_max]CallFrame = undefined,
    framesCount: usize = 0,
    stack: [stack_max]Value = undefined,
    stack_top: [*]Value = undefined,
    strings: Table,
    initString: ?*Object.String = null,
    globals: Table,
    objects: ?*Object = null,
    openUpvalues: ?*Object.Upvalue = null,
    allocator: Allocator,
    collector: ?*GarbageCollector = null,
    parser: ?*Parser = null,

    pub fn init(allocator: Allocator) Self {
        var vm = Self{ .allocator = allocator, .strings = Table.init(allocator), .globals = Table.init(allocator) };
        vm.stack_top = vm.stack[0..];
        vm.initString = Object.String.copy(&vm, "init");
        vm.defineNative("clock", native.clockNative);
        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();
        self.strings.deinit();
        self.initString = null;
        self.collector.?.freeObjects();
        self.collector.?.deinit();
    }

    pub fn enableGC(self: *Self, collector: *GarbageCollector) void {
        self.collector = collector;
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
        const function = compile(self, source) catch return InterpretError.CompileError;
        self.push(Value.ObjectValue(&function.object));
        const closure = Object.Closure.create(self, function);
        _ = self.pop();
        self.push(Value.ObjectValue(&closure.object));
        _ = self.call(closure, 0);
        try self.run();
    }

    fn currentChunk(self: *Self) *Chunk {
        return &self.currentFrame().closure.function.chunk;
    }

    fn currentFrame(self: *Self) *CallFrame {
        return &self.frames[self.framesCount - 1];
    }

    pub fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime debug_stack_execution) {
                debug.printStack(self.stack[0..self.stack_top]);
            }
            if (comptime debug_trace_execution) {
                _ = debug.disassembleInstruction(self.currentChunk(), self.currentFrame().ip);
            }

            const instruction = self.readInstruction();

            try switch (instruction) {
                .op_constant => self.push(self.readConstant()),
                .op_nil => self.push(Value.NilValue()),
                .op_true => self.push(Value.BooleanValue(true)),
                .op_false => self.push(Value.BooleanValue(false)),
                .op_equal => {
                    const boxed_lhs = self.pop();
                    const boxed_rhs = self.pop();

                    self.push(Value.BooleanValue(boxed_lhs.equal(boxed_rhs)));
                },
                .op_greater => self.compOp(.gt),
                .op_less => self.compOp(.lt),
                .op_negate => {
                    const boxed = self.pop();

                    switch (boxed) {
                        .boolean, .nil, .object => {
                            self.runtimeError("Operand must be a number", .{});
                            return InterpretError.RuntimeError;
                        },
                        .number => |val| self.push(Value.NumberValue(-val)),
                    }
                },
                .op_not => self.push(Value.BooleanValue(isFalsey(self.pop()))),
                .op_add => {
                    if (Object.isA(self.peek(0), .String) and Object.isA(self.peek(1), .String)) {
                        self.concatenate();
                    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        const rhs = self.pop().number;
                        const lhs = self.pop().number;

                        self.push(Value.NumberValue(lhs + rhs));
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings", .{});
                        return InterpretError.RuntimeError;
                    }
                },
                .op_subtract => self.binaryOp(.sub),
                .op_multiply => self.binaryOp(.mul),
                .op_divide => self.binaryOp(.div),
                .op_print => printValue(self.pop()),
                .op_pop => _ = self.pop(),
                .op_define_global => {
                    const name = self.readConstant().object.asString();
                    _ = self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                .op_get_global => {
                    const name = self.readConstant().object.asString();
                    const value = self.globals.get(name) orelse {
                        self.runtimeError("Undefined variable '{s}'", .{name.chars});
                        return InterpretError.RuntimeError;
                    };

                    self.push(value.*);
                },
                .op_set_global => {
                    const name = self.readConstant().object.asString();
                    if (self.globals.set(name, self.peek(0))) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'", .{name.chars});
                        return InterpretError.RuntimeError;
                    }
                },
                .op_get_local => {
                    const slot = self.readByte();
                    self.push(self.currentFrame().slots[slot]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    self.currentFrame().slots[slot] = self.peek(0);
                },
                .op_get_upvalue => {
                    const slot = self.readByte();
                    self.push(self.currentFrame().closure.upvalues[slot].location.*);
                },
                .op_set_upvalue => {
                    const slot = self.readByte();
                    self.currentFrame().closure.upvalues[slot].location.* = self.peek(0);
                },
                .op_close_upvalue => {
                    self.closeUpvalue(self.stack_top - 1);
                    _ = self.pop();
                },
                .op_jump => {
                    const offset = self.readTwoBytes();
                    self.currentFrame().ip += offset;
                },
                .op_jump_if_false => {
                    const offset = self.readTwoBytes();
                    if (isFalsey(self.peek(0))) self.currentFrame().ip += offset;
                },
                .op_loop => {
                    const offset = self.readTwoBytes();
                    self.currentFrame().ip -= offset;
                },
                .op_call => {
                    const argCount = self.readByte();
                    if (!self.callValue(self.peek(argCount), argCount)) return InterpretError.RuntimeError;
                },
                .op_closure => {
                    const function = self.readConstant().object.asFunction();
                    const closure = Object.Closure.create(self, function);

                    self.push(Value.ObjectValue(&closure.object));
                    var i: usize = 0;
                    while (i < closure.upvalueCount) : (i += 1) {
                        const isLocal = self.readByte() == 1;
                        const index = self.readByte();

                        if (isLocal) {
                            closure.upvalues[i] = self.captureUpvalue(&self.currentFrame().slots[index]);
                        } else {
                            closure.upvalues[i] = self.currentFrame().closure.upvalues[index];
                        }
                    }
                },
                .op_class => {
                    const className = self.readConstant().object.asString();
                    const class = Object.Class.create(self, className);
                    self.push(Value.ObjectValue(&class.object));
                },
                .op_inherit => {
                    const superclass = self.peek(1);

                    if (!Object.isA(superclass, .Class)) {
                        self.runtimeError("Superclass must be a class", .{});
                        return InterpretError.RuntimeError;
                    }

                    const subclass = self.peek(0).object.asClass();
                    superclass.object.asClass().methods.addAll(&subclass.methods);
                    _ = self.pop(); // subclass
                },
                .op_get_super => {
                    const name = self.readConstant().object.asString();
                    const superclass = self.pop().object.asClass();

                    if (!self.bindMethod(superclass, name)) {
                        return InterpretError.RuntimeError;
                    }
                },
                .op_super_invoke => {
                    const method = self.readConstant().object.asString();
                    const argCount = self.readByte();
                    const superclass = self.pop().object.asClass();

                    if (!self.invokeFromClass(superclass, method, argCount)) {
                        return InterpretError.RuntimeError;
                    }
                },
                .op_get_property => {
                    if (!Object.isA(self.peek(0), .Instance)) {
                        self.runtimeError("Only instances have properties", .{});
                        return InterpretError.RuntimeError;
                    }

                    const instance = self.peek(0).object.asInstance();
                    const name = self.readConstant().object.asString();

                    if (instance.fields.get(name)) |value| {
                        _ = self.pop();
                        self.push(value.*);
                    } else if (!self.bindMethod(instance.class, name)) {
                        return InterpretError.RuntimeError;
                    }
                },
                .op_set_property => {
                    if (!Object.isA(self.peek(1), .Instance)) {
                        self.runtimeError("Only instances have fields", .{});
                        return InterpretError.RuntimeError;
                    }

                    const instance = self.peek(1).object.asInstance();
                    const name = self.readConstant().object.asString();

                    _ = instance.fields.set(name, self.peek(0));

                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },
                .op_method => self.defineMethod(self.readConstant().object.asString()),
                .op_invoke => {
                    const method = self.readConstant().object.asString();
                    const argCount = self.readByte();

                    if (!self.invoke(method, argCount)) {
                        return InterpretError.RuntimeError;
                    }
                },
                .op_return => {
                    const result = self.pop();
                    const frame = self.currentFrame();
                    self.closeUpvalue(frame.slots);
                    self.framesCount -= 1;

                    if (self.framesCount == 0) return;

                    self.stack_top = frame.slots;
                    self.push(result);
                },
            };
        }
    }

    fn resetStack(self: *Self) void {
        self.stack_top = self.stack[0..];
        self.framesCount = 0;
        self.openUpvalues = null;
    }

    pub inline fn push(self: *Self, value: Value) void {
        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    inline fn peek(self: *Self, back: usize) Value {
        return (self.stack_top - 1 - back)[0];
    }

    fn callValue(self: *Self, callee: Value, argCount: u8) bool {
        switch (callee) {
            .object => |object| {
                switch (object.objectType) {
                    .NativeFunction => return self.callNative(object.asNativeFunction(), argCount),
                    .Closure => return self.call(object.asClosure(), argCount),
                    .Class => {
                        const class = object.asClass();
                        const instance = Object.Instance.create(self, class);

                        (self.stack_top - argCount - 1)[0] = Value.ObjectValue(&instance.object);

                        if (class.methods.get(self.initString.?)) |initializer| {
                            return self.call(initializer.object.asClosure(), argCount);
                        } else if (argCount != 0) {
                            self.runtimeError("Expected 0 arguments but got {d}", .{argCount});
                            return false;
                        }

                        return true;
                    },
                    .BoundMethod => {
                        const boundMethod = object.asBoundMethod();
                        (self.stack_top - argCount - 1)[0] = boundMethod.receiver;
                        return self.call(boundMethod.method, argCount);
                    },
                    else => {
                        self.runtimeError("Can only call functions and classes", .{});
                        return false;
                    },
                }
            },
            else => {
                self.runtimeError("Can only call functions and classes", .{});
                return false;
            },
        }
    }

    fn invoke(self: *Self, name: *Object.String, argCount: u8) bool {
        const receiver = self.peek(argCount);

        if (!Object.isA(receiver, .Instance)) {
            self.runtimeError("Only instances have methods", .{});
            return false;
        }

        const instance = receiver.object.asInstance();

        if (instance.fields.get(name)) |value| {
            (self.stack_top - argCount - 1)[0] = value.*;
            return self.callValue(value.*, argCount);
        }

        return self.invokeFromClass(instance.class, name, argCount);
    }

    fn invokeFromClass(self: *Self, class: *Object.Class, name: *Object.String, argCount: u8) bool {
        if (class.methods.get(name)) |method| {
            return self.call(method.object.asClosure(), argCount);
        } else {
            self.runtimeError("Undefined property '{s}'", .{name.chars});
            return false;
        }
    }

    fn call(self: *Self, closure: *Object.Closure, argCount: u8) bool {
        const function = closure.function;

        if (function.arity != argCount) {
            self.runtimeError("Expected {d} arguments but got {d}", .{ function.arity, argCount });
            return false;
        }

        if (self.framesCount == frames_max) {
            self.runtimeError("Stack overflow", .{});
            return false;
        }

        const frame = &self.frames[self.framesCount];
        self.framesCount += 1;

        frame.closure = closure;
        frame.ip = closure.function.chunk.code.items.ptr;
        frame.slots = self.stack_top - argCount - 1;

        return true;
    }

    fn callNative(self: *Self, function: *Object.NativeFunction, argCount: u8) bool {
        const args = (self.stack_top - argCount - 1)[0];
        const result = function.function(args);
        self.stack_top -= argCount + 1;
        self.push(result);

        return true;
    }

    fn captureUpvalue(self: *Self, local: *Value) *Object.Upvalue {
        var prevUpvalue: ?*Object.Upvalue = null;
        var maybeUpvalue = self.openUpvalues;

        while (maybeUpvalue) |upvalue| {
            if (@intFromPtr(upvalue.location) <= @intFromPtr(local)) break;
            prevUpvalue = upvalue;
            maybeUpvalue = upvalue.next;
        }

        if (maybeUpvalue) |upvalue| {
            if (upvalue.location == local) return upvalue;
        }

        const createdUpvalue = Object.Upvalue.create(self, local);
        createdUpvalue.next = maybeUpvalue;

        if (prevUpvalue == null) {
            self.openUpvalues = createdUpvalue;
        } else {
            prevUpvalue.?.next = createdUpvalue;
        }

        return createdUpvalue;
    }

    fn closeUpvalue(self: *Self, last: [*]Value) void {
        while (self.openUpvalues) |openUpvalues| {
            if (@intFromPtr(openUpvalues.location) < @intFromPtr(last)) break;
            const upvalue = openUpvalues;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
    }

    pub inline fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    inline fn readInstruction(self: *Self) OpCode {
        return OpCode.fromU8(self.readByte());
    }

    inline fn readByte(self: *Self) u8 {
        const frame = self.currentFrame();
        const byte = frame.ip[0];
        frame.ip += 1;
        return byte;
    }

    inline fn readTwoBytes(self: *Self) u16 {
        const b1 = self.readByte();
        const b2 = self.readByte();
        return (@as(u16, @intCast(b1)) << 8) | @as(u16, @intCast(b2));
    }

    inline fn readConstant(self: *Self) Value {
        const constant = self.currentChunk().constants.items[self.readByte()];
        return constant;
    }

    fn runtimeError(self: *Self, comptime message: []const u8, args: anytype) void {
        @setCold(true);

        const err_writer = std.io.getStdErr().writer();

        err_writer.print(message ++ ".\n", args) catch {};

        var i = self.framesCount;
        while (i > 0) {
            i -= 1;

            const frame = &self.frames[i];
            const function = frame.closure.function;
            const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr) - 1;

            err_writer.print("[line {d}] in ", .{function.chunk.lines.items[instruction]}) catch {};
            const name = if (function.name) |name| name.chars else "script";
            err_writer.print("{s}\n", .{name}) catch {};
        }

        self.resetStack();
    }

    fn defineNative(self: *Self, name: []const u8, function: Object.NativeFunction.Fn) void {
        self.push(Value.ObjectValue(&Object.String.copy(self, name).object));
        self.push(Value.ObjectValue(&Object.NativeFunction.create(self, function).object));

        _ = self.globals.set(self.stack[0].object.asString(), self.stack[1]);

        _ = self.pop();
        _ = self.pop();
    }

    fn defineMethod(self: *Self, name: *Object.String) void {
        const method = self.peek(0);
        const class = self.peek(1).object.asClass();
        _ = class.methods.set(name, method);
        _ = self.pop();
    }

    fn bindMethod(self: *Self, class: *Object.Class, name: *Object.String) bool {
        if (class.methods.get(name)) |method| {
            const boundMethod = Object.BoundMethod.create(self, self.peek(0), method.object.asClosure());
            _ = self.pop();
            self.push(Value.ObjectValue(&boundMethod.object));
            return true;
        } else {
            self.runtimeError("Undefined property '{s}'", .{name.chars});
            return false;
        }
    }

    inline fn binaryOp(self: *Self, op: BinaryOp) InterpretError!void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers", .{});
            return InterpretError.RuntimeError;
        }

        const rhs = self.pop().number;
        const lhs = self.pop().number;

        const result = switch (op) {
            .add => lhs + rhs,
            .sub => lhs - rhs,
            .mul => lhs * rhs,
            .div => lhs / rhs,
        };

        self.push(Value.NumberValue(result));
    }

    fn concatenate(self: *Self) void {
        const rhs = self.peek(0).object.asString();
        const lhs = self.peek(1).object.asString();

        const heap = std.mem.concat(self.allocator, u8, &[_][]const u8{ lhs.chars, rhs.chars }) catch unreachable;
        const obj = Object.String.take(self, heap);

        _ = self.pop();
        _ = self.pop();

        self.push(Value.ObjectValue(&obj.object));
    }

    inline fn compOp(self: *Self, op: CompOp) InterpretError!void {
        const boxed_rhs = self.pop();
        const boxed_lhs = self.pop();

        if (!boxed_lhs.isNumber() or !boxed_rhs.isNumber()) {
            self.runtimeError("Operands must be numbers", .{});
            return InterpretError.RuntimeError;
        }

        const result = switch (op) {
            .gt => boxed_lhs.number > boxed_rhs.number,
            .lt => boxed_lhs.number < boxed_rhs.number,
        };

        self.push(Value.BooleanValue(result));
    }
};

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .boolean => |val| !val,
        else => false,
    };
}
