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
    ip: usize = 0,
    slots: usize = 0,
};

pub const Vm = struct {
    const Self = @This();

    frames: [frames_max]CallFrame = undefined,
    framesCount: usize = 0,
    stack: [stack_max]Value = undefined,
    stack_top: usize = 0,
    strings: Table,
    globals: Table,
    objects: ?*Object = null,
    openUpvalues: ?*Object.Upvalue = null,
    allocator: Allocator,
    collector: ?*GarbageCollector = null,

    pub fn init(allocator: Allocator) Self {
        var vm = Self{ .allocator = allocator, .strings = Table.init(allocator), .globals = Table.init(allocator) };
        vm.defineNative("clock", native.clockNative);
        return vm;
    }

    pub fn deinit(self: *Self) void {
        self.collector.?.freeObjects();
        self.globals.deinit();
        self.strings.deinit();
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
                .op_add => self.binaryOp(.add),
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
                    self.push(self.stack[self.currentFrame().slots + slot]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    self.stack[self.currentFrame().slots + slot] = self.peek(0);
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
                    self.closeUpvalue(&self.stack[self.stack_top - 1]);
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
                            closure.upvalues[i] = self.captureUpvalue(&self.stack[self.currentFrame().slots + index]);
                        } else {
                            closure.upvalues[i] = self.currentFrame().closure.upvalues[index];
                        }
                    }
                },
                .op_return => {
                    const result = self.pop();
                    const frame = self.currentFrame();
                    self.closeUpvalue(&self.stack[frame.slots]);
                    self.framesCount -= 1;

                    if (self.framesCount == 0) return;

                    self.stack_top = frame.slots;
                    self.push(result);
                },
            };
        }
    }

    fn resetStack(self: *Self) void {
        self.stack_top = 0;
        self.framesCount = 0;
        self.openUpvalues = null;
    }

    inline fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn peek(self: *Self, back: usize) Value {
        return self.stack[self.stack_top - 1 - back];
    }

    inline fn callValue(self: *Self, callee: Value, argCount: u8) bool {
        switch (callee) {
            .object => |object| {
                switch (object.objectType) {
                    //.Function => return self.call(object.asFunction(), argCount),
                    .NativeFunction => return self.callNative(object.asNativeFunction(), argCount),
                    .Closure => return self.call(object.asClosure(), argCount),
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

    inline fn call(self: *Self, closure: *Object.Closure, argCount: u8) bool {
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
        frame.ip = 0;
        frame.slots = self.stack_top - argCount - 1;

        return true;
    }

    inline fn callNative(self: *Self, function: *Object.NativeFunction, argCount: u8) bool {
        const args = self.stack[self.stack_top - argCount - 1];
        const result = function.function(args);
        self.stack_top -= argCount + 1;
        self.push(result);

        return true;
    }

    inline fn captureUpvalue(self: *Self, local: *Value) *Object.Upvalue {
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

    fn closeUpvalue(self: *Self, last: *Value) void {
        while (self.openUpvalues) |openUpvalues| {
            if (@intFromPtr(openUpvalues.location) < @intFromPtr(last)) break;
            const upvalue = openUpvalues;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.openUpvalues = upvalue.next;
        }
    }

    inline fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    inline fn readInstruction(self: *Self) OpCode {
        return OpCode.fromU8(self.readByte());
    }

    inline fn readByte(self: *Self) u8 {
        const byte = self.currentChunk().code.items[self.currentFrame().ip];
        self.currentFrame().ip += 1;
        return byte;
    }

    inline fn readTwoBytes(self: *Self) u16 {
        const b1 = @as(u16, self.currentChunk().code.items[self.currentFrame().ip]);
        const b2 = self.currentChunk().code.items[self.currentFrame().ip + 1];
        self.currentFrame().ip += 2;
        return (b1 << 8) | b2;
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
            const instruction = frame.ip - 1;

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

    inline fn binaryOp(self: *Self, op: BinaryOp) InterpretError!void {
        const boxed_rhs = self.pop();
        const boxed_lhs = self.pop();

        switch (boxed_lhs) {
            .boolean, .nil => {
                self.runtimeError("Operands must be two numbers or two strings", .{});
                return InterpretError.RuntimeError;
            },
            .number => |lhs| {
                switch (boxed_rhs) {
                    .boolean, .nil => {
                        self.runtimeError("Operands must be two numbers or two strings", .{});
                        return InterpretError.RuntimeError;
                    },
                    .object => {
                        self.runtimeError("Operands must be numbers", .{});
                        return InterpretError.RuntimeError;
                    },
                    .number => |rhs| {
                        const result = switch (op) {
                            .add => lhs + rhs,
                            .sub => lhs - rhs,
                            .mul => lhs * rhs,
                            .div => lhs / rhs,
                        };

                        self.push(Value.NumberValue(result));
                    },
                }
            },
            .object => |lhs| {
                switch (boxed_rhs) {
                    .boolean, .nil => {
                        self.runtimeError("Operands must be two numbers or two strings", .{});
                        return InterpretError.RuntimeError;
                    },
                    .number => {
                        self.runtimeError("Operands must be numbers", .{});
                        return InterpretError.RuntimeError;
                    },
                    .object => |rhs| {
                        switch (lhs.objectType) {
                            .Function, .NativeFunction, .Closure, .Upvalue => {
                                self.runtimeError("Operands must be two numbers or two strings", .{});
                                return InterpretError.RuntimeError;
                            },
                            .String => switch (rhs.objectType) {
                                .Function, .NativeFunction, .Closure, .Upvalue => {
                                    self.runtimeError("Operands must be two numbers or two strings", .{});
                                    return InterpretError.RuntimeError;
                                },
                                .String => {
                                    switch (op) {
                                        .add => {
                                            const heap = std.mem.concat(self.allocator, u8, &[_][]const u8{ lhs.asString().chars, rhs.asString().chars }) catch unreachable;
                                            const obj = Object.String.take(self, heap);

                                            self.push(Value.ObjectValue(&obj.object));
                                        },
                                        else => unreachable,
                                    }
                                },
                            },
                        }
                    },
                }
            },
        }
    }

    inline fn compOp(self: *Self, op: CompOp) InterpretError!void {
        const boxed_rhs = self.pop();
        const boxed_lhs = self.pop();

        switch (boxed_lhs) {
            .boolean, .nil, .object => {
                self.runtimeError("Operands must be numbers", .{});
                return InterpretError.RuntimeError;
            },
            .number => |lhs| {
                switch (boxed_rhs) {
                    .boolean, .nil, .object => {
                        self.runtimeError("Operands must be numbers", .{});
                        return InterpretError.RuntimeError;
                    },
                    .number => |rhs| {
                        const result = switch (op) {
                            .gt => lhs > rhs,
                            .lt => lhs < rhs,
                        };

                        self.push(Value.BooleanValue(result));
                    },
                }
            },
        }
    }
};

fn isFalsey(value: Value) bool {
    return switch (value) {
        .nil => true,
        .boolean => |val| !val,
        else => false,
    };
}
