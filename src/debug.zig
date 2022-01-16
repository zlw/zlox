const std = @import("std");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value  = @import("./value.zig").Value;

pub fn disassembleChunk(chunk: *Chunk, comptime name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});

    const code = chunk.code;
    var offset: usize = 0;

    while (offset < code.count) {
        disassembleInstruction(chunk, offset);
        offset = calcOffset(code.items[offset], offset);
    }
}

fn calcOffset(instruction_code: u8, current_offset: usize) usize {
    const instruction = OpCode.fromU8(instruction_code);
    return current_offset + instruction.operands() + 1;
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) void {
    std.debug.print("{d:0>4} ", .{offset});

    const code = chunk.code;
    const lines = chunk.lines;

    if (offset > 0 and lines.items[offset] == lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{lines.items[offset]});
    }

    const instruction = OpCode.fromU8(code.items[offset]);

    switch(instruction) {
        .op_constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .op_return   => simpleInstruction("OP_RETURN"),
    }
}

fn simpleInstruction(comptime name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) void {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];

    std.debug.print("{s: <16} '{}'\n", .{ name, constant });
}

pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
}


pub fn printStack(stack: []Value) void {
    std.debug.print("          ", .{});

    for (stack) |value| {
        std.debug.print("[{}]", .{value});
    }

    std.debug.print("\n", .{});
}
