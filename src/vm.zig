const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("./debug.zig");

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const compile = @import("./compiler.zig").compile;

const debug_trace_execution = true;
const debug_stack_execution = false;
const stack_max = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
};

pub const Vm = struct {
    const Self = @This();

    chunk: *Chunk,
    ip: usize,
    stack: [stack_max]Value,
    stack_top: usize,
    allocator: *Allocator,

    pub fn init(allocator: *Allocator) Self {
        return Self{ .ip = 0, .stack_top = 0, .stack = undefined, .chunk = undefined, .allocator = allocator };
    }

    pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
        _ = self;
        compile(source);

        return;
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

            switch (instruction) {
                .op_constant => {
                    self.push(self.readConstant());
                },
                .op_negate => {
                    self.push(-self.pop());
                },
                .op_add => {
                    self.push(self.pop() + self.pop());
                },
                .op_subtract => {
                    self.push(self.pop() - self.pop());
                },
                .op_multiply => {
                    self.push(self.pop() * self.pop());
                },
                .op_divide => {
                    self.push(self.pop() / self.pop());
                },
                .op_return => {
                    debug.printValue(self.pop());
                    std.debug.print("\n", .{});

                    return;
                },
            }
        }
    }

    pub fn resetStack(self: *Self) void {
        self.stack_top = 0;
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
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
};
