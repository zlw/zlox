const std = @import("std");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value  = @import("./value.zig").Value;

pub const debug_trace_execution = false;
pub const debug_stack_execution = false;
pub const debug_rule_selection = false;

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
        .op_nil => simpleInstruction("OP_NIL"),
        .op_false => simpleInstruction("OP_FALSE"),
        .op_true => simpleInstruction("OP_TRUE"),
        .op_negate   => simpleInstruction("OP_NEGATE"),
        .op_not => simpleInstruction("OP_NOT"),
        .op_equal => simpleInstruction("OP_EQUAL"),
        .op_greater => simpleInstruction("OP_GREATER"),
        .op_less => simpleInstruction("OP_LESS"),
        .op_add      => simpleInstruction("OP_ADD"),
        .op_subtract => simpleInstruction("OP_SUBTRACT"),
        .op_multiply => simpleInstruction("OP_MULTIPLY"),
        .op_divide   => simpleInstruction("OP_DIVIDE"),
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

pub fn printStack(stack: []Value) void {
    std.debug.print("          ", .{});

    for (stack) |value| {
        std.debug.print("[{}]", .{value});
    }

    std.debug.print("\n", .{});
}
