const std = @import("std");
const scanner = @import("lib/scanner.zig");
const parser = @import("lib/parser.zig");
const interpreter = @import("lib/interpreter.zig");
const llvm = @import("lib/llvm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var args = try std.process.ArgIterator.initWithAllocator(allocator);

    _ = args.next();

    var inputFile: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (inputFile == null) {
            inputFile = arg;
        } else {
            return error.ExtraArg;
        }
    }

    if (inputFile == null) return error.NoFile;

    var buff = try allocator.alloc(u8, 1000000);
    var contsLen = try (try std.fs.cwd().openFile(inputFile.?, .{})).readAll(buff);

    var scn = scanner.Scanner.init(inputFile.?, buff[0..contsLen]);
    var psr = parser.Parser.init(scn, allocator);
    var root = try psr.parse();

    //var data = try std.fmt.allocPrint(allocator, "{}\n", .{root});
    //var dataFile = try std.fs.cwd().createFile("AST", .{});
    //_ = try dataFile.write(data);

    var inter = interpreter.Interpreter.init(allocator, root);
    inter.run() catch |err| {
        var str = inter.module.printToString();

        var file = try std.fs.cwd().createFile("ir", .{});

        var writing: []const u8 = undefined;
        writing.ptr = @ptrCast([*]const u8, str);
        writing.len = 1;
        while (writing[writing.len - 1] != 0) writing.len += 1;
        writing.len -= 1;

        _ = try file.write(writing);

        return err;
    };

    var str = inter.module.printToString();

    var file = try std.fs.cwd().createFile("ir", .{});

    var writing: []const u8 = undefined;
    writing.ptr = @ptrCast([*]const u8, str);
    writing.len = 1;
    while (writing[writing.len - 1] != 0) writing.len += 1;
    writing.len -= 1;

    _ = try file.write(writing);

    std.debug.print("LLVM lol.o\n", .{});

    const CPU: [*:0]const u8 = "x86-64";
    const features: [*:0]const u8 = "";
    const thriple: [*:0]const u8 = "x86_64-linux.6.3.4...6.3.4-gnu.2.36";
    const out: [*:0]const u8 = "lol.o";
    var opt: ?*llvm.RelocMode = null;
    var t: *llvm.Target = undefined;
    var err: [*:0]const u8 = @ptrCast([*:0]const u8, try allocator.alloc(u8, 512));
    if (llvm.Target.getFromTriple(thriple, &t, &err).toBool()) {
        std.log.info("{s}", .{err});
    }

    var targetMachine = llvm.TargetMachine.create(t, thriple, CPU, features, opt, .None);

    targetMachine.emitToFile(inter.module, out, .ObjectFile);

    std.debug.print("CC a.out\n", .{});

    var output = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "clang", "lol.o", "-lm", "-lc", "-lGL", "-lglfw", "test.c", "-fstack-protector-all", "-fno-omit-frame-pointer", "-O0" },
    });

    if (output.stdout.len != 0)
        std.log.info("{s}", .{output.stdout});
    if (output.stderr.len != 0)
        std.log.err("{s}", .{output.stderr});
}
