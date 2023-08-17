const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("./object.zig");
const debug = @import("./debug.zig");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

const compile = @import("./compiler.zig").compile;

const debug_trace_execution = debug.debug_trace_execution;
const debug_stack_execution = debug.debug_stack_execution;
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
    allocator: *Allocator,

    pub fn init(allocator: *Allocator) Self {
        return Self{ .allocator = allocator };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        compile(self, source, &chunk) catch return InterpretError.CompileError;

        self.chunk = &chunk;
        self.ip = 0;

        return self.run();
    }

    pub fn run(self: *Self) InterpretError!void {
        while (true) {
            if (comptime debug_stack_execution) {
                debug.printStack(self.stack[0..self.stack_top]);
            }
            if (comptime debug_trace_execution) {
                debug.disassembleInstruction(self.chunk, self.ip);
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
                .op_return => {
                    printValue(self.pop());
                    std.debug.print("\n", .{});

                    return;
                },
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

    pub inline fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    inline fn readInstruction(self: *Self) OpCode {
        const instruction = OpCode.fromU8(self.chunk.code.items[self.ip]);
        self.ip += 1;
        return instruction;
    }

    inline fn readConstant(self: *Self) Value {
        const constant = self.chunk.constants.items[self.chunk.code.items[self.ip]];
        self.ip += 1;
        return constant;
    }

    fn runtimeError(self: *Self, comptime message: []const u8, args: anytype) void {
        const err_writer = std.io.getStdErr().writer();

        err_writer.print(message ++ "\n", args) catch {};

        err_writer.print("[line {d}] in script.\n", .{self.chunk.lines.items[self.ip]}) catch {};

        self.resetStack();
    }

    inline fn binaryOp(self: *Self, op: BinaryOp) InterpretError!void {
        const boxed_lhs = self.pop();
        const boxed_rhs = self.pop();

        switch (boxed_lhs) {
            .boolean, .nil, .object => {
                self.runtimeError("Operand must be a number", .{});
                return InterpretError.RuntimeError;
            },
            .number => |lhs| {
                switch (boxed_rhs) {
                    .boolean, .nil, .object => {
                        self.runtimeError("Operand must be a number", .{});
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
        }
    }

    inline fn compOp(self: *Self, op: CompOp) InterpretError!void {
        const boxed_lhs = self.pop();
        const boxed_rhs = self.pop();

        switch (boxed_lhs) {
            .boolean, .nil, .object => {
                self.runtimeError("Operand must be a number", .{});
                return InterpretError.RuntimeError;
            },
            .number => |lhs| {
                switch (boxed_rhs) {
                    .boolean, .nil, .object => {
                        self.runtimeError("Operand must be a number", .{});
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
