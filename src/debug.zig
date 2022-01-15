const std = @import("std");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

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

    return switch(@intToEnum(OpCode, chunk.code.items[offset])) {
        .op_constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .op_return   => simpleInstruction("OP_RETURN", offset),
    };
}

fn simpleInstruction(comptime name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});

    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];

    std.debug.print("{s: <16} '{}'\n", .{ name, constant });

    return offset + 2;
}
