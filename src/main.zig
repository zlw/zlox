const std = @import("std");
const Allocator = std.mem.Allocator;
const io = std.io;
const process = std.process;

const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Vm = @import("./vm.zig").Vm;
const InterpretError = @import("./vm.zig").InterpretError;

const errout = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn main() anyerror!u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    var vm = Vm.init(&allocator);

    switch (args.len) {
        1 => repl(&vm),
        2 => runFile(args[0], &vm, allocator),
        else => {
            const stderr = io.getStdErr().writer();
            try stderr.print("Usage: lox [path]\n", .{});
            process.exit(64);
        },
    }

    return 0;
}

fn repl(vm: *Vm) void {
    var buf = std.io.bufferedReader(stdin);
    var reader = buf.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        stdout.writeAll("> ") catch std.debug.panic("Couldn't write to stdout you have serious problems", .{});
        var line = reader.readUntilDelimiterOrEof(&line_buf, '\n') catch {
            std.debug.panic("Couldn't read from stdin in repl you have serious problems", .{});
        } orelse {
            stdout.writeAll("\n") catch std.debug.panic("Couldn't write to stdout you have serious problems", .{});
            break;
        };

        vm.interpret(line) catch {};
    }
}

fn runFile(fileName: []const u8, vm: *Vm, allocator: Allocator) void {
    const source = readFile(fileName, allocator);
    defer allocator.free(source);

    vm.interpret(source) catch |e| {
        switch (e) {
            InterpretError.RuntimeError => std.process.exit(70),
            InterpretError.CompileError => std.process.exit(65),
        }
    };
}

fn readFile(path: []const u8, allocator: Allocator) []const u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        errout.print("Could not open file \"{s}\", error: {any}.\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
    defer file.close();

    return file.readToEndAlloc(allocator, 100_000_000) catch |err| {
        errout.print("Could not read file \"{s}\", error: {any}.\n", .{ path, err }) catch {};
        std.process.exit(74);
    };
}

test "basic test" {
    try std.testing.expectEqual(10, 3 + 7);
}
