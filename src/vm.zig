const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const value = @import("./value.zig");
const Value = value.Value;

pub const InterpretError = error {
    CompileError,
    RuntimeError,
};

pub const Vm = struct {
    const Self = @This();

    chunk: *Chunk,
    ip: usize,
    allocator: *Allocator,

    pub fn init(allocator: *Allocator) Self {
        return Self {
            .ip = 0,
            .chunk = undefined,
            .allocator = allocator
        };
    }

    pub fn interpret(self: *Self, chunk: *Chunk) InterpretError!void {
        self.chunk = chunk;

        try self.run();
    }

    pub fn run(self: *Self) InterpretError!void {
        while(true) {
            const instruction = self.readInstruction();

            switch(instruction) {
                .op_constant => {
                    const constant = self.readConstant();
                    value.printValue(constant);
                    std.debug.print("\n", .{});
                    break;
                },
                .op_return => return,
            }
        }
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
