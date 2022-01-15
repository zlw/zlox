const std   = @import("std");
const debug = @import("./debug.zig");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var chunk = Chunk.init(&allocator);
    defer _   = chunk.deinit();

    const constant = chunk.addConstant(1.2);
    chunk.write(OpCode.op_constant.toU8(), 123);
    chunk.write(@intCast(u8, constant), 123);
    chunk.write(OpCode.op_return.toU8(), 123);

    debug.disassembleChunk(&chunk, "test chunk");

    return 0;
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
