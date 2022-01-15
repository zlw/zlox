const std = @import("std");
const chunks = @import("./chunk.zig");

const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;

pub fn disassembleChunk(chunk: *Chunk, comptime name: []const u8) void {
    std.debug.print("=== {s} ===\n", .{name});

    const code = chunk.code;

    var offset: usize = 0;
    while (offset < code.count) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});

    const code = chunk.code;

    const instruction = @intToEnum(OpCode, code.items[offset]);

    switch(instruction) {
        .op_return => simpleInstruction("OP_RETURN"),
    }

    return offset + 1;
}

fn simpleInstruction(comptime name: []const u8) void {
    std.debug.print("{s}\n", .{name});
}
