const std = @import("std");
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const Value = @import("./value.zig").Value;
const Vm = @import("./vm.zig").Vm;

const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    const Self = @This();

    op_constant,
    op_nil,
    op_true,
    op_false,
    op_negate,
    op_not,
    op_equal,
    op_greater,
    op_less,
    op_add,
    op_subtract,
    op_multiply,
    op_divide,
    op_print,
    op_pop,
    op_define_global,
    op_get_global,
    op_set_global,
    op_get_local,
    op_set_local,
    op_get_upvalue,
    op_set_upvalue,
    op_close_upvalue,
    op_jump,
    op_jump_if_false,
    op_loop,
    op_call,
    op_closure,
    op_class,
    op_return,

    pub fn toU8(self: Self) u8 {
        return @intFromEnum(self);
    }

    pub fn fromU8(n: u8) Self {
        return @enumFromInt(n);
    }
};

pub const Chunk = struct {
    const Self = @This();

    const BytesArray = DynamicArray(u8);
    pub const ValueArray = DynamicArray(Value);
    const LinesArray = DynamicArray(usize);

    vm: *Vm,
    code: BytesArray,
    constants: ValueArray,
    lines: LinesArray,

    pub fn init(allocator: Allocator, vm: *Vm) Chunk {
        return Self{
            .vm = vm,
            .code = BytesArray.init(allocator),
            .constants = ValueArray.init(allocator),
            .lines = LinesArray.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) !void {
        self.code.appendItem(byte);
        self.lines.appendItem(line);
    }

    pub fn addConstant(self: *Self, value: Value) !u16 {
        self.vm.push(value);

        self.constants.appendItem(value);

        _ = self.vm.pop();

        return @as(u16, @intCast(self.constants.count - 1));
    }
};

test "create a Chunk with bytes only" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = Vm.init(allocator);
    defer vm.deinit();
    var chunk = Chunk.init(allocator, &vm);
    defer chunk.deinit();

    try chunk.write(OpCode.op_return.toU8(), 1);
    try expect(chunk.code.items[0] == OpCode.op_return.toU8());

    try chunk.write(OpCode.op_return.toU8(), 1);
    try chunk.write(OpCode.op_return.toU8(), 1);
    try chunk.write(OpCode.op_return.toU8(), 1);
    try chunk.write(OpCode.op_return.toU8(), 1);
    try chunk.write(OpCode.op_return.toU8(), 1);

    try expect(chunk.code.items[4] == OpCode.op_return.toU8());
    chunk.deinit();
}
