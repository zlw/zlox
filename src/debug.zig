const std = @import("std");

const Chunk  = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value  = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

pub const debug_trace_execution = false;
pub const debug_stack_execution = false;
pub const debug_rule_selection = false;
pub const debug_garbage_collection = false;
pub const debug_print_code = false;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
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
    const lines = chunk.lines;

    if (offset > 0 and lines.items[offset] == lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d: >4} ", .{lines.items[offset]});
    }

    const instruction = OpCode.fromU8(code.items[offset]);

    return switch(instruction) {
        .op_constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .op_nil => simpleInstruction("OP_NIL", offset),
        .op_false => simpleInstruction("OP_FALSE", offset),
        .op_true => simpleInstruction("OP_TRUE", offset),
        .op_negate   => simpleInstruction("OP_NEGATE", offset),
        .op_not => simpleInstruction("OP_NOT", offset),
        .op_equal => simpleInstruction("OP_EQUAL", offset),
        .op_greater => simpleInstruction("OP_GREATER", offset),
        .op_less => simpleInstruction("OP_LESS", offset),
        .op_add      => simpleInstruction("OP_ADD", offset),
        .op_subtract => simpleInstruction("OP_SUBTRACT", offset),
        .op_multiply => simpleInstruction("OP_MULTIPLY", offset),
        .op_divide   => simpleInstruction("OP_DIVIDE", offset),
        .op_print => simpleInstruction("OP_PRINT", offset),
        .op_pop => simpleInstruction("OP_POP", offset),
        .op_define_global => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .op_get_global => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .op_set_global => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .op_get_local => byteInstruction("OP_GET_LOCAL", chunk, offset),
        .op_set_local => byteInstruction("OP_SET_LOCAL", chunk, offset),
        .op_jump => jumpInstruction("OP_JUMP", 1, chunk, offset),
        .op_jump_if_false => jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        .op_loop => jumpInstruction("OP_LOOP", -1, chunk, offset),
        .op_call => byteInstruction("OP_CALL", chunk, offset),
        .op_invoke => invokeInstruction("OP_INVOKE", chunk, offset),
        .op_closure => closureInstruction("OP_CLOSURE", chunk, offset),
        .op_get_upvalue => byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .op_set_upvalue => byteInstruction("OP_SET_UPVALUE", chunk, offset),
        .op_close_upvalue => simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .op_class => constantInstruction("OP_CLASS", chunk, offset),
        .op_get_property => constantInstruction("OP_GET_PROPERTY", chunk, offset),
        .op_set_property => constantInstruction("OP_SET_PROPERTY", chunk, offset),
        .op_inherit => simpleInstruction("OP_INHERIT", offset),
        .op_method => constantInstruction("OP_METHOD", chunk, offset),
        .op_return => simpleInstruction("OP_RETURN", offset),
    };
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant_idx = chunk.code.items[offset + 1];
    const constant = chunk.constants.items[constant_idx];

    std.debug.print("{s: <16} '{}'\n", .{ name, constant });
    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot = chunk.code.items[offset + 1];
    std.debug.print("{s} {d} \n", .{name, slot});
    std.debug.print("\n", .{});
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize) usize {
    var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];
    const target = @as(isize, @intCast(offset)) + 3 + sign * @as(isize, @intCast(jump));
    std.debug.print("{s} {d} -> {d}\n", .{name, offset, target});
    return offset + 3;
}

fn closureInstruction(name: []const u8, chunk: *Chunk, initialOffset: usize) usize {
    var offset = initialOffset + 1;
    const constant = chunk.code.items[offset];
    offset += 1;
    std.debug.print("{s} {} ", .{ name, constant });
    printValue(chunk.constants.items[constant]);
    std.debug.print("\n", .{});

    // Disassemble upvalues
    const function = chunk.constants.items[constant].object.asFunction();
    var i: usize = 0;
    while (i < function.upvalueCount) : (i += 1) {
        const isLocal = chunk.code.items[offset] != 1;
        const valueType = if (isLocal) "local" else "upvalue";
        offset += 1;
        const index = chunk.code.items[offset];
        offset += 1;
        std.debug.print("{} | {s} {}\n", .{ offset - 2, valueType, index});
    }

    return offset;
}

fn invokeInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    const argCount = chunk.code.items[offset + 2];
    std.debut.print("{s} ({d} args) {d} '", name, argCount, constant);

    printValue(chunk.constants.items[constant]);
    std.debug.print("' \n", .{});

    return offset + 3;
}

pub fn printStack(stack: []Value) void {
    std.debug.print("          ", .{});

    for (stack) |value| {
        std.debug.print("[{}]", .{value});
    }

    std.debug.print("\n", .{});
}
