const std   = @import("std");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Vm     = @import("./vm.zig").Vm;

pub fn main() anyerror!u8 {
    var gpa       = std.heap.GeneralPurposeAllocator(.{}){};
    defer _       = gpa.deinit();
    var allocator = gpa.allocator();

    var vm    = Vm.init(&allocator);

    var chunk = Chunk.init(&allocator);
    defer _   = chunk.deinit();

    var constant = chunk.addConstant(1.2);
    chunk.write(OpCode.op_constant.toU8(), 1);
    chunk.write(@intCast(u8, constant), 1);

    constant = chunk.addConstant(3.4);
    chunk.write(OpCode.op_constant.toU8(), 1);
    chunk.write(@intCast(u8, constant), 1);

    chunk.write(OpCode.op_add.toU8(), 1);

    constant = chunk.addConstant(5.6);
    chunk.write(OpCode.op_constant.toU8(), 1);
    chunk.write(@intCast(u8, constant), 1);

    chunk.write(OpCode.op_divide.toU8(), 1);
    chunk.write(OpCode.op_negate.toU8(), 1);
    chunk.write(OpCode.op_return.toU8(), 1);

    try vm.interpret(&chunk);

    return 0;
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
