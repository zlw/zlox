const std = @import("std");
const chunk = @import("./chunk.zig");
const dynamic_array = @import("./dynamic_array.zig");
const debug = @import("./debug.zig");

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var ret = chunk.Chunk.init(&allocator);
    defer _ = ret.deinit();
    ret.write(chunk.OpCode.op_return.toU8());

    debug.disassembleChunk(&ret, "test chunk");

    return 0;
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
