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

const debug_trace_execution = debug.debug_trace_execution;
const debug_stack_execution = debug.debug_stack_execution;
const debug_garbage_collection = debug.debug_garbage_collection;
const stack_max = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

const BinaryOp = enum { add, sub, mul, div };
const CompOp = enum { gt, lt };

pub const Vm = struct {
    const Self = @This();

    chunk: *Chunk = undefined,
    ip: usize = 0,
    stack: [stack_max]Value = undefined,
    stack_top: usize = 0,
    strings: Table,
    globals: Table,
    objects: ?*Object = null,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator, .strings = Table.init(allocator), .globals = Table.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.freeObjects();
        self.globals.deinit();
        self.strings.deinit();
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
        const function = compile(self, source) catch return InterpretError.CompileError;

        self.chunk = &function.chunk;
        self.ip = 0;

        return self.run();
    }

    pub fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime debug_stack_execution) {
                debug.printStack(self.stack[0..self.stack_top]);
            }
            if (comptime debug_trace_execution) {
                _ = debug.disassembleInstruction(self.chunk, self.ip);
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
                    const slot = self.readInstruction().toU8();
                    self.push(self.stack[slot]);
                },
                .op_set_local => {
                    const slot = self.readInstruction().toU8();
                    self.stack[slot] = self.peek(0);
                },
                .op_jump => {
                    const offset = self.readTwoBytes();
                    self.ip += offset;
                },
                .op_jump_if_false => {
                    const offset = self.readTwoBytes();
                    if (isFalsey(self.peek(0))) self.ip += offset;
                },
                .op_loop => {
                    const offset = self.readTwoBytes();
                    self.ip -= offset;                    
                },
                .op_return => return,
            };
        }
    }

    pub fn resetStack(self: *Self) void {
        self.stack_top = 0;
    }

    pub inline fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn peek(self: *Self, back: usize) Value {
        return self.stack[self.stack_top - 1 - back];
    }
    pub inline fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    inline fn readInstruction(self: *Self) OpCode {
        return OpCode.fromU8(self.readByte());
    }

    inline fn readByte(self: *Self) u8 {
        const byte = self.chunk.code.items[self.ip];
        self.ip += 1;
        return byte;
    }

    inline fn readTwoBytes(self: *Self) u16 {
        const b1 = @as(u16, self.chunk.code.items[self.ip]);
        const b2 = self.chunk.code.items[self.ip + 1];
        self.ip += 2;
        return (b1 << 8) | b2;
    }

    inline fn readConstant(self: *Self) Value {
        const constant = self.chunk.constants.items[self.chunk.code.items[self.ip]];
        self.ip += 1;
        return constant;
    }

    fn runtimeError(self: *Self, comptime message: []const u8, args: anytype) void {
        const err_writer = std.io.getStdErr().writer();

        err_writer.print("[line {d}] Error in script: ", .{self.chunk.lines.items[self.ip]}) catch {};
        err_writer.print(message ++ ".\n", args) catch {};

        self.resetStack();
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
                        switch (op) {
                            .add => {
                                const heap = std.mem.concat(self.allocator, u8, &[_][]const u8{ lhs.asString().chars, rhs.asString().chars }) catch unreachable;
                                const obj = Object.String.take(self, heap);

                                self.push(Value.ObjectValue(&obj.object));
                            },
                            else => unreachable,
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

    fn freeObjects(self: *Self) void {
        var obj = self.objects;
        var total_objects: u64 = 0;

        while (obj) |object| {
            if (comptime debug_garbage_collection) {
                total_objects += 1;
            }
            const next = object.next;
            object.destroy(self);
            obj = next;
        }

        if (comptime debug_garbage_collection) {
            std.debug.print("Objects freed {d}\n", .{total_objects});
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
