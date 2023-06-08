const std = @import("std");
const parser = @import("parser.zig");
const llvm = @import("llvm.zig");

pub const interpreterError = error{
    NoMain,
    OutOfMemory,
    InvalidFunc,
    InvalidCast,
    InvalidProp,
    InvalidStatement,
    Undefined,
    Unimplemented,
    BadIndex,
};

pub const Interpreter = struct {
    const Self = @This();

    const ValueKind = enum {
        Null,
        Val,
        ConstInt,
        Array,
        Ptr,
        Type,
        Prop,
        Struct,
        ValProc,
        Proc,
    };

    const DefList = std.StringHashMap(*Value);

    const ProcImpl = struct {
        value: FunctionData,
        args: []*Value,
    };

    const Value = union(ValueKind) {
        Null: u0,
        Val: struct {
            value: *llvm.Value,
            kind: *Value,
        },
        ConstInt: struct {
            value: usize,
        },
        Array: struct {
            child: *Value,
            kind: *llvm.Type,
            count: usize,
        },
        Ptr: struct {
            child: *Value,
            kind: *llvm.Type,
        },
        Type: struct {
            value: *llvm.Type,
        },
        Prop: struct {
            idx: usize,
            kind: *Value,
        },
        Struct: struct {
            value: *llvm.Type,
            subDefs: DefList,
        },
        ValProc: struct {
            val: *Value,
            proc: *Value,
        },
        Proc: struct {
            parentDefs: *DefList,
            def: parser.Definition,
            impls: std.ArrayList(ProcImpl),
            ext: bool,
            inl: bool,
            ctime: bool,
            name: []const u8,
        },

        pub fn sameAs(self: *const Value, other: *const Value) bool {
            if (@enumToInt(self.*) != @enumToInt(other.*)) return false;

            switch (self.*) {
                .Array => return self.Array.child.sameAs(other.Array.child),
                .Ptr => return self.Ptr.child.sameAs(other.Ptr.child),
                .Type => return self.Type.value == other.Type.value,
                .Struct => return self.Struct.value == other.Struct.value,

                else => return self == other,
            }

            return true;
        }

        pub fn getValue(self: Value, targetType: *llvm.Type) !*llvm.Value {
            return switch (self) {
                .Val => return self.Val.value,
                .ValProc => return try self.ValProc.val.getValue(targetType),
                .Null => return targetType.constNull(),
                .ConstInt => {
                    return llvm.Type.constInt(targetType, self.ConstInt.value, .False);
                },
                else => return error.InvalidProp,
            };
        }

        pub fn getTypeVal(self: Value) !*llvm.Type {
            return switch (self) {
                .Ptr => self.Ptr.kind,
                .Array => self.Array.kind,
                .Type => self.Type.value,
                .Struct => self.Struct.value,
                else => return error.InvalidProp,
            };
        }
    };

    const FunctionData = struct {
        value: ?*llvm.Value,
        kind: ?*llvm.Type,
        retKind: ?*Value,

        out: ?*llvm.Value,
        outBlock: ?*llvm.BasicBlock,
        ctime: ?*Value,
        name: []const u8,
    };

    context: *llvm.Context,
    module: *llvm.Module,
    builder: *llvm.Builder,

    allocator: std.mem.Allocator,

    definitions: DefList,
    strings: std.StringHashMap(*Value),

    root: parser.Definition,

    pub fn visitStatement(self: *Self, stmt: parser.Statement, func: ?*FunctionData, defs: *DefList) interpreterError!void {
        switch (stmt.data) {
            .Definition => |data| {
                if (func == null) return error.InvalidStatement;

                try self.visitDefinition(func, defs, null, data, false, func.?.name);
            },
            .Expression => |data| {
                _ = try self.visitExpression(func, data, defs);
            },
            .Return => |data| {
                if (func == null) return error.InvalidStatement;

                if (func.?.ctime) |ctime| {
                    var returns = try self.visitExpression(func, data, defs);
                    ctime.* = returns.*;

                    return;
                }

                if (func.?.out != null) {
                    var returns = try self.visitExpression(func.?, data, defs);
                    _ = self.builder.buildStore(try returns.getValue(try func.?.retKind.?.getTypeVal()), func.?.out.?);

                    _ = self.builder.buildBr(func.?.outBlock.?);

                    return;
                }

                var returns = try self.visitExpression(func, data, defs);
                _ = self.builder.buildRet(try returns.getValue(try func.?.retKind.?.getTypeVal()));

                return;
            },
            .While => |data| {
                if (func == null) return error.InvalidStatement;

                var headBB = self.context.appendBasicBlock(func.?.value.?, "whileHead");
                var bodyBB = self.context.appendBasicBlock(func.?.value.?, "whileBody");
                var mergeBB = self.context.appendBasicBlock(func.?.value.?, "whileMerge");

                _ = self.builder.buildBr(headBB);
                self.builder.positionBuilderAtEnd(headBB);

                var condV = try (try self.visitExpression(func, data.check, defs)).getValue(self.context.intType(1));

                condV = self.builder.buildICmp(.NE, condV, llvm.Type.constInt(self.context.intType(1), 0, .False), "whileCond");

                _ = self.builder.buildCondBr(condV, bodyBB, mergeBB);
                self.builder.positionBuilderAtEnd(bodyBB);

                for (data.body) |subStmt| {
                    try self.visitStatement(subStmt, func, defs);
                }

                _ = self.builder.buildBr(headBB);
                self.builder.positionBuilderAtEnd(mergeBB);
            },
            .If => |data| {
                if (func == null) return error.InvalidStatement;

                if (data.bodyElse) |bodyElse| {
                    var condV = try (try self.visitExpression(func, data.check, defs)).getValue(self.context.intType(1));

                    condV = self.builder.buildICmp(.NE, condV, llvm.Type.constInt(self.context.intType(1), 0, .False), "ifCond");

                    var bodyBB = self.context.appendBasicBlock(func.?.value.?, "ifBody");
                    var elseBB = self.context.appendBasicBlock(func.?.value.?, "ifElse");
                    var mergeBB = self.context.appendBasicBlock(func.?.value.?, "ifMerge");

                    _ = self.builder.buildCondBr(condV, bodyBB, elseBB);
                    self.builder.positionBuilderAtEnd(bodyBB);

                    for (data.body) |subStmt| {
                        try self.visitStatement(subStmt, func, defs);
                    }

                    _ = self.builder.buildBr(mergeBB);
                    self.builder.positionBuilderAtEnd(elseBB);

                    for (bodyElse) |subStmt| {
                        try self.visitStatement(subStmt, func, defs);
                    }

                    _ = self.builder.buildBr(mergeBB);
                    self.builder.positionBuilderAtEnd(mergeBB);

                    return;
                }

                var condV = try (try self.visitExpression(func, data.check, defs)).getValue(self.context.intType(1));

                condV = self.builder.buildICmp(.NE, condV, llvm.Type.constInt(self.context.intType(1), 0, .False), "ifcond");

                var bodyBB = self.context.appendBasicBlock(func.?.value.?, "ifBody");
                var mergeBB = self.context.appendBasicBlock(func.?.value.?, "ifMerge");

                _ = self.builder.buildCondBr(condV, bodyBB, mergeBB);
                self.builder.positionBuilderAtEnd(bodyBB);

                for (data.body) |subStmt| {
                    try self.visitStatement(subStmt, func, defs);
                }

                _ = self.builder.buildBr(mergeBB);
                self.builder.positionBuilderAtEnd(mergeBB);
            },
        }
    }

    pub fn visitExpression(self: *Self, func: ?*FunctionData, expr: parser.Expression, defs: *DefList) interpreterError!*Value {
        switch (expr.data) {
            .ConstInt => {
                var result = try self.allocator.create(Value);
                result.* = .{
                    .ConstInt = .{
                        .value = expr.data.ConstInt.value,
                    },
                };

                return result;
            },
            .ConstString => |data| {
                if (self.strings.get(data.value)) |result| {
                    return result;
                }

                var finalStr = try self.allocator.alloc(u8, data.value.len);

                var idx: usize = 0;
                var fidx: usize = 0;

                while (idx < data.value.len) {
                    if (data.value[idx] == '\\') {
                        idx += 1;
                        switch (data.value[idx]) {
                            'n' => {
                                finalStr[fidx] = '\n';
                            },
                            't' => {
                                finalStr[fidx] = '\t';
                            },
                            'e' => {
                                finalStr[fidx] = '\x1b';
                            },
                            'r' => {
                                finalStr[fidx] = '\r';
                            },
                            else => {
                                finalStr[fidx] = data.value[idx];
                            },
                        }
                    } else {
                        finalStr[fidx] = data.value[idx];
                    }
                    idx += 1;
                    fidx += 1;
                }

                var strVal = self.context.constString(finalStr.ptr, @intCast(c_uint, fidx), .False);

                var str = self.module.addGlobal(strVal.typeOf(), "str");
                str.setInitializer(strVal);

                var cKind = try self.allocator.create(Value);
                cKind.* = .{
                    .Type = .{
                        .value = self.context.intType(8),
                    },
                };

                var kind = try self.allocator.create(Value);
                kind.* = .{
                    .Ptr = .{
                        .kind = self.context.pointerType(0),
                        .child = cKind,
                    },
                };

                var result = try self.allocator.create(Value);
                result.* = .{
                    .Val = .{
                        .value = str,
                        .kind = kind,
                    },
                };

                try self.strings.put(data.value, result);

                return result;
            },
            .Ident => |data| {
                if (data.name[0] == 'i' or data.name[0] == 'u') {
                    if (std.fmt.parseInt(c_uint, data.name[1..], 0) catch null) |val| {
                        var result = try self.allocator.create(Value);
                        result.* = .{
                            .Type = .{
                                .value = self.context.intType(val),
                            },
                        };

                        return result;
                    }
                }

                if (std.mem.eql(u8, data.name, "null")) {
                    var result = try self.allocator.create(Value);
                    result.* = .{
                        .Null = 0,
                    };

                    return result;
                }

                if (defs.get(data.name)) |def| {
                    return def;
                }

                //if (self.definitions.get("root")) |root| {
                //    if (root.Struct.subDefs.get(data.name)) |def| {
                //        return def;
                //    }
                //}

                std.log.info("'{s}'", .{data.name});

                return error.Undefined;
            },
            .Operation => |data| {
                switch (data.op) {
                    .Assign => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildStore(try b.getValue(try a.Val.kind.Ptr.child.getTypeVal()), a.Val.value),
                            },
                        };

                        return result;
                    },
                    .IndexAccess => {
                        var result = try self.visitExpression(func, data.values[0], defs);

                        for (data.values[1..]) |*param| {
                            var index = try self.visitExpression(func, param.*, defs);

                            switch (result.*) {
                                .Val => {
                                    if (result.Val.kind.* != .Ptr) return error.BadIndex;

                                    switch (result.Val.kind.Ptr.child.*) {
                                        .Ptr => {
                                            var ind: []const *llvm.Value = &.{try index.*.getValue(self.context.intType(32))};

                                            var kind = try self.allocator.create(Value);

                                            kind.* = .{
                                                .Ptr = .{
                                                    .child = result.Val.kind.Ptr.child.Ptr.child,
                                                    .kind = self.context.pointerType(0),
                                                },
                                            };

                                            var copy = result;
                                            result = try self.allocator.create(Value);

                                            result.* = .{
                                                .Val = .{
                                                    .value = self.builder.buildInBoundsGEP(
                                                        try copy.Val.kind.Ptr.child.Ptr.child.getTypeVal(),
                                                        copy.Val.value,
                                                        ind.ptr,
                                                        1,
                                                        "AccessPtr",
                                                    ),
                                                    .kind = kind,
                                                },
                                            };

                                            kind.Ptr.kind = result.Val.value.typeOf();
                                        },
                                        .Array => {
                                            var ind: []const *llvm.Value = &.{try index.*.getValue(self.context.intType(32))};

                                            var kind = try self.allocator.create(Value);

                                            kind.* = .{
                                                .Ptr = .{
                                                    .child = result.Val.kind.Ptr.child.Array.child,
                                                    .kind = self.context.pointerType(0),
                                                },
                                            };

                                            var copy = result;
                                            result = try self.allocator.create(Value);

                                            result.* = .{
                                                .Val = .{
                                                    .value = self.builder.buildInBoundsGEP(
                                                        try copy.Val.kind.Ptr.child.Array.child.getTypeVal(),
                                                        copy.Val.value,
                                                        ind.ptr,
                                                        1,
                                                        "AccessArr",
                                                    ),
                                                    .kind = kind,
                                                },
                                            };

                                            kind.Ptr.kind = result.Val.value.typeOf();
                                        },
                                        .Type => {
                                            var ind: []const *llvm.Value = &.{try index.*.getValue(self.context.intType(32))};

                                            var kind = try self.allocator.create(Value);

                                            kind.* = .{
                                                .Ptr = .{
                                                    .child = result.Val.kind.Ptr.child,
                                                    .kind = self.context.pointerType(0),
                                                },
                                            };

                                            var copy = result;
                                            result = try self.allocator.create(Value);

                                            result.* = .{
                                                .Val = .{
                                                    .value = (try copy.Val.kind.Ptr.child.getTypeVal()).arrayType(1).constInBoundsGEP(
                                                        copy.Val.value,
                                                        ind.ptr,
                                                        1,
                                                    ),
                                                    .kind = kind,
                                                },
                                            };
                                            kind.Ptr.kind = result.Val.value.typeOf();
                                        },
                                        else => return error.BadIndex,
                                    }
                                },
                                .Type => {
                                    if (index.* != .ConstInt) return error.BadIndex;

                                    var copy = result;
                                    result = try self.allocator.create(Value);

                                    result.* = .{
                                        .Array = .{
                                            .child = copy,
                                            .kind = (try copy.getTypeVal()).arrayType(@intCast(c_uint, index.ConstInt.value)),
                                            .count = index.ConstInt.value,
                                        },
                                    };
                                },
                                else => return error.BadIndex,
                            }
                        }

                        return result;
                    },
                    .Call => {
                        var function = try self.visitExpression(func, data.values[0], defs);

                        if (function.* == .Type) {
                            if (data.values.len != 2) return error.InvalidCast;

                            var toCast = try (try self.visitExpression(func, data.values[1], defs)).getValue(try function.getTypeVal());

                            var result = try self.allocator.create(Value);

                            switch (function.Type.value.getTypeKind()) {
                                .Integer => {
                                    switch (toCast.typeOf().getTypeKind()) {
                                        .Integer => {
                                            result.* = .{
                                                .Val = .{
                                                    .value = self.builder.buildIntCast2(toCast, function.Type.value, .True, "IntCast"),
                                                    .kind = function,
                                                },
                                            };

                                            return result;
                                        },
                                        .Pointer => {
                                            result.* = .{
                                                .Val = .{
                                                    .value = self.builder.buildPtrToInt(toCast, function.Type.value, "IntCast"),
                                                    .kind = function,
                                                },
                                            };

                                            return result;
                                        },
                                        else => {},
                                    }
                                },
                                else => {},
                            }
                            std.log.info("{s}", .{@tagName(function.Type.value.getTypeKind())});
                            return error.InvalidCast;
                        }

                        var paramCount = data.values.len - 1;
                        var offset: usize = 1;

                        if (function.* == .ValProc) {
                            paramCount = paramCount + 1;
                            offset = 0;
                            function = function.ValProc.proc;
                        }

                        var params = try self.allocator.alloc(*Value, paramCount);
                        var paramTypes = try self.allocator.alloc(*Value, paramCount);
                        var callParams = try self.allocator.alloc(*llvm.Value, paramCount);
                        var idx: usize = 0;

                        for (params, paramTypes, 0..) |*param, *ptype, aidx| {
                            param.* = try self.visitExpression(func, data.values[aidx + offset], defs);
                            if (param.*.* == .ConstInt) {
                                ptype.* = try self.allocator.create(Value);
                                ptype.*.* = .{
                                    .Type = .{
                                        .value = self.context.intType(32),
                                    },
                                };
                            } else if (param.*.* == .ValProc) {
                                ptype.* = param.*.ValProc.val.Val.kind;
                                param.* = param.*.ValProc.val;
                            } else if (param.*.* == .Val) {
                                ptype.* = param.*.Val.kind;
                            } else if (param.*.* == .Null) {
                                ptype.* = try self.allocator.create(Value);
                                ptype.*.* = .{
                                    .Type = .{
                                        .value = self.context.pointerType(0),
                                    },
                                };

                                param.*.* = .{
                                    .Val = .{
                                        .value = self.context.pointerType(0).constNull(),
                                        .kind = ptype.*,
                                    },
                                };
                            } else {
                                continue;
                            }

                            callParams[idx] = try param.*.getValue(self.context.intType(32));

                            idx += 1;
                        }

                        callParams.len = idx;

                        var functionData = try self.implement(func, function, function.Proc.name, params, function.Proc.parentDefs);

                        if (functionData.ctime) |ctime| {
                            return ctime;
                        }

                        if (functionData.out) |out| {
                            var kindData = try self.allocator.create(Value);
                            kindData.* = functionData.retKind.?.*;

                            var result = try self.allocator.create(Value);
                            result.* = .{
                                .Val = .{
                                    .value = out,
                                    .kind = kindData,
                                },
                            };

                            return result;
                        }

                        var kindData = try self.allocator.create(Value);
                        kindData.* = functionData.retKind.?.*;

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .value = self.builder.buildCall(functionData.kind.?, functionData.value.?, callParams.ptr, @intCast(c_uint, callParams.len), "call"),
                                .kind = kindData,
                            },
                        };

                        return result;
                    },
                    .Deref => {
                        var a = try self.visitExpression(func, data.values[0], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .Type or a.* == .Ptr or a.* == .Array or a.* == .Struct) {
                            result.* = .{
                                .Ptr = .{
                                    .kind = self.context.pointerType(0),
                                    .child = a,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind.Ptr.child,
                                .value = self.builder.buildLoad(try a.Val.kind.Ptr.child.getTypeVal(), a.Val.value, "deref"),
                            },
                        };

                        return result;
                    },
                    .Div => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildSDiv(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "mul"),
                            },
                        };

                        return result;
                    },
                    .Mod => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildSRem(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "mul"),
                            },
                        };

                        return result;
                    },
                    .Mul => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = a.ConstInt.value * b.ConstInt.value,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildMul(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "mul"),
                            },
                        };

                        return result;
                    },
                    .Add => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = a.ConstInt.value + b.ConstInt.value,
                                },
                            };

                            return result;
                        }

                        var adds = try b.getValue(try a.Val.kind.getTypeVal());

                        if ((try a.Val.kind.getTypeVal()).getTypeKind() == .Pointer) {
                            adds = self.builder.buildIntToPtr(adds, self.context.pointerType(0), "addAddr");
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildAdd(a.Val.value, adds, "add"),
                            },
                        };

                        return result;
                    },
                    .BitOr => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = a.ConstInt.value | b.ConstInt.value,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildOr(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "or"),
                            },
                        };

                        return result;
                    },
                    .BitAnd => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = a.ConstInt.value & b.ConstInt.value,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildAnd(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "and"),
                            },
                        };

                        return result;
                    },
                    .BitNot => {
                        var a = try self.visitExpression(func, data.values[0], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = ~a.ConstInt.value,
                                },
                            };
                        } else {
                            result.* = .{
                                .Val = .{
                                    .kind = a.Val.kind,
                                    .value = self.builder.buildNot(a.Val.value, "not"),
                                },
                            };
                        }

                        return result;
                    },
                    .Sub => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = a.ConstInt.value -% b.ConstInt.value,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildSub(a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "sub"),
                            },
                        };

                        return result;
                    },
                    .Equal => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        if (a.* == .ConstInt and b.* == .ConstInt) {
                            result.* = .{
                                .ConstInt = .{
                                    .value = if (a.ConstInt.value == b.ConstInt.value) 1 else 0,
                                },
                            };

                            return result;
                        }

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildICmp(.EQ, a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "eq"),
                            },
                        };

                        return result;
                    },
                    .NotEqual => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildICmp(.NE, a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "ne"),
                            },
                        };

                        return result;
                    },
                    .Less => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildICmp(.SLT, a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "slt"),
                            },
                        };

                        return result;
                    },
                    .Greater => {
                        var a = try self.visitExpression(func, data.values[0], defs);
                        var b = try self.visitExpression(func, data.values[1], defs);

                        var result = try self.allocator.create(Value);

                        result.* = .{
                            .Val = .{
                                .kind = a.Val.kind,
                                .value = self.builder.buildICmp(.SGT, a.Val.value, try b.getValue(try a.Val.kind.getTypeVal()), "sgt"),
                            },
                        };

                        return result;
                    },
                    .Access => {
                        var a = try self.visitExpression(func, data.values[0], defs);

                        if (std.mem.eql(u8, data.values[1].data.Ident.name, "SIZE")) {
                            var resulta = try self.allocator.create(Value);

                            var ind: []const *llvm.Value = &.{self.context.intType(32).constInt(1, .False)};

                            var subV = try self.allocator.create(Value);

                            subV.* = .{
                                .Type = .{
                                    .value = self.context.intType(32),
                                },
                            };

                            resulta.* = .{
                                .Val = .{
                                    .value = self.builder.buildPtrToInt(
                                        (try a.getTypeVal()).arrayType(1).constInBoundsGEP(
                                            self.context.pointerType(0).constNull(),
                                            ind.ptr,
                                            1,
                                        ),
                                        self.context.intType(32),
                                        "SIZE",
                                    ),
                                    .kind = subV,
                                },
                            };

                            return resulta;
                        }

                        if (a.* == .Struct) {
                            if (a.Struct.subDefs.get(data.values[1].data.Ident.name)) |result| {
                                return result;
                            }

                            std.log.info("{s}", .{data.values[1].data.Ident.name});

                            return error.Undefined;
                        }

                        if (a.* == .Val) {
                            if (a.Val.kind.* != .Ptr) return error.Undefined;

                            var def = a.Val.kind.Ptr.child.Struct.subDefs.get(data.values[1].data.Ident.name) orelse {
                                std.log.info("{s}", .{data.values[1].data.Ident.name});

                                return error.InvalidProp;
                            };

                            if (def.* == .Prop) {
                                var prop = def.Prop;

                                var result = self.builder.buildStructGEP(a.Val.kind.Ptr.child.Struct.value, a.Val.value, @intCast(c_uint, prop.idx), try self.allocator.dupeZ(u8, data.values[1].data.Ident.name));

                                var subV = try self.allocator.create(Value);

                                subV.* = .{
                                    .Ptr = .{
                                        .child = prop.kind,
                                        .kind = self.context.pointerType(5),
                                    },
                                };

                                var resulta = try self.allocator.create(Value);

                                resulta.* = .{
                                    .Val = .{
                                        .value = result,
                                        .kind = subV,
                                    },
                                };

                                return resulta;
                            }

                            if (def.* == .Proc) {
                                var resulta = try self.allocator.create(Value);

                                resulta.* = .{
                                    .ValProc = .{
                                        .val = a,
                                        .proc = def,
                                    },
                                };

                                return resulta;
                            }
                        }

                        std.log.info("{s}", .{data.values[1].data.Ident.name});

                        return error.InvalidProp;
                    },
                    else => {
                        std.log.info("{s}", .{@tagName(data.op)});

                        return error.Unimplemented;
                    },
                }
            },
            .Paren => |data| {
                return try self.visitExpression(func, data.expr.*, defs);
            },
        }
    }

    pub fn visitDefinition(self: *Self, func: ?*FunctionData, parent: *DefList, propIdx: ?*usize, def: parser.Definition, root: bool, rootName: []const u8) interpreterError!void {
        var name = try std.mem.concat(self.allocator, u8, &.{ rootName, if (rootName.len == 0) "" else "_", def.name });
        var zname = try std.mem.concatWithSentinel(self.allocator, u8, &.{ rootName, if (rootName.len == 0) "" else "_", def.name }, 0);
        if (std.mem.eql(u8, name, "root")) {
            name = "";
            zname[0] = 0;
        }

        switch (def.data) {
            .Var => |data| {
                var kind = try self.visitExpression(func, data.kind, parent);

                var kindType = try kind.getTypeVal();

                var subType = try self.allocator.create(Value);

                subType.* = .{
                    .Ptr = .{
                        .child = kind,
                        .kind = self.context.pointerType(0),
                    },
                };

                var value = try self.allocator.create(Value);

                value.* = if (root) blk: {
                    var res = Value{
                        .Val = .{
                            .value = self.module.addGlobal(kindType, zname),
                            .kind = subType,
                        },
                    };

                    res.Val.value.setInitializer((try kind.getTypeVal()).constNull());

                    break :blk res;
                } else Value{
                    .Val = .{
                        .value = self.builder.buildAlloca(kindType, zname),
                        .kind = subType,
                    },
                };

                try parent.put(def.name, value);
                return;
            },
            .Const => |data| {
                var value = try self.visitExpression(func, data.value, parent);

                try parent.put(def.name, value);

                return;
            },
            .Prop => |data| {
                if (propIdx == null) return error.InvalidProp;

                var kind = try self.visitExpression(func, data.kind, parent);

                var value = try self.allocator.create(Value);

                value.* = Value{
                    .Prop = .{
                        .idx = propIdx.?.*,
                        .kind = kind,
                    },
                };

                propIdx.?.* += 1;

                try parent.put(def.name, value);
                return;
            },
            .Proc => {
                var value = try self.allocator.create(Value);
                var ctime = if (def.data.Proc.out.data == .Ident)
                    std.mem.eql(u8, def.data.Proc.out.data.Ident.name, "CTIME")
                else
                    false;

                value.* = .{
                    .Proc = .{
                        .parentDefs = parent,
                        .def = def,
                        .impls = std.ArrayList(ProcImpl).init(self.allocator),
                        .ext = false,
                        .inl = def.data.Proc.inl,
                        .ctime = ctime,
                        .name = name,
                    },
                };

                try parent.put(def.name, value);

                return;
            },
            .Struct => |data| {
                var value = try self.allocator.create(Value);
                value.* = .{
                    .Struct = .{
                        .value = self.context.structCreateNamed(zname),
                        .subDefs = DefList.init(self.allocator),
                    },
                };

                try parent.put(def.name, value);

                var subDefs = &((parent.get(def.name) orelse unreachable).Struct.subDefs);

                var iter = parent.iterator();

                while (iter.next()) |defin| {
                    if (defin.value_ptr.*.* != .Prop) {
                        try subDefs.put(defin.key_ptr.*, defin.value_ptr.*);
                    }
                }

                if (parent.get("Self") != null)
                    try subDefs.put("Parent", parent.get("Self") orelse unreachable);

                try subDefs.put("Self", value);

                var valuePropIdx: usize = 0;

                for (data.subDefs) |subdef| {
                    try self.visitDefinition(null, subDefs, &valuePropIdx, subdef, root, name);
                }

                iter = subDefs.iterator();

                var props = try self.allocator.alloc(*llvm.Type, valuePropIdx);

                while (iter.next()) |entry| {
                    if (entry.value_ptr.*.* == .Prop) {
                        props[entry.value_ptr.*.Prop.idx] = try entry.value_ptr.*.Prop.kind.getTypeVal();
                    }
                }

                value.Struct.value.structSetBody(props.ptr, @intCast(c_uint, props.len), .False);

                return;
            },
            .Extern => {
                var value = try self.allocator.create(Value);
                value.* = .{
                    .Proc = .{
                        .name = def.name,
                        .parentDefs = parent,
                        .def = .{
                            .name = def.name,
                            .data = .{
                                .Proc = .{
                                    .in = def.data.Extern.in,
                                    .out = def.data.Extern.out,
                                    .insts = undefined,
                                    .inl = false,
                                },
                            },
                        },
                        .impls = std.ArrayList(ProcImpl).init(self.allocator),
                        .ext = true,
                        .inl = false,
                        .ctime = false,
                    },
                };

                try parent.put(def.name, value);

                return;
            },
        }
    }

    pub fn implement(self: *Self, parent: ?*FunctionData, def: *Value, name: []const u8, args: []const *Value, predefs: ?*DefList) interpreterError!FunctionData {
        if (def.* != .Proc) return error.InvalidFunc;

        for (def.Proc.impls.items) |impl| {
            if (def.Proc.ext) {
                return impl.value;
            }

            if (impl.args.len != args.len) continue;

            for (impl.args, args) |foreign, arg| blk: {
                if (!foreign.sameAs(arg)) {
                    break :blk;
                }
            } else {
                return impl.value;
            }
        }

        var subDefs = DefList.init(self.allocator);
        defer subDefs.deinit();

        if (predefs) |defs| {
            var iter = defs.iterator();

            while (iter.next()) |defin| {
                try subDefs.put(defin.key_ptr.*, defin.value_ptr.*);
            }
        }

        if (def.Proc.ctime) {
            std.log.info("CTIME - {s}", .{name});

            var copy = try self.allocator.create(FunctionData);
            copy.* = .{
                .value = null,
                .kind = null,
                .retKind = null,
                .out = null,
                .outBlock = null,
                .ctime = try self.allocator.create(Value),
                .name = name,
            };

            for (def.Proc.def.data.Proc.in, 0..) |paramName, idx| {
                try subDefs.put(paramName, args[idx]);
            }

            for (def.Proc.def.data.Proc.insts) |stmt| {
                try self.visitStatement(stmt, copy, &subDefs);
            }

            std.log.info("Done - {s}", .{name});

            return copy.*;
        }

        var out = try self.visitExpression(undefined, def.Proc.def.data.Proc.out, &subDefs);
        var kind = try out.getTypeVal();

        if (def.Proc.inl and parent != null) {
            std.log.info("Inline - {s}", .{name});
            var outData = self.builder.buildAlloca(kind, try self.allocator.dupeZ(u8, name));
            var resultBB = self.context.appendBasicBlock(parent.?.value.?, "inlineResult");

            var copy = try self.allocator.create(FunctionData);
            copy.* = parent.?.*;

            copy.retKind = out;
            copy.out = outData;
            copy.outBlock = resultBB;

            for (def.Proc.def.data.Proc.in, 0..) |paramName, idx| {
                try subDefs.put(paramName, args[idx]);
            }

            for (def.Proc.def.data.Proc.insts) |stmt| {
                try self.visitStatement(stmt, copy, &subDefs);
            }

            self.builder.positionBuilderAtEnd(resultBB);

            copy.out = self.builder.buildLoad(kind, copy.out.?, "inlineDeref");

            return copy.*;
        }

        std.log.info("Impl - {s}", .{name});

        var argsTypes = try self.allocator.alloc(*llvm.Type, args.len);

        var isVarArg = false;
        var argIdx: usize = 0;

        for (args) |arg| {
            if (arg.* == .ConstInt) {
                argsTypes[argIdx] = (try arg.getValue(self.context.intType(32))).typeOf();
            } else if (arg.* == .Val) {
                argsTypes[argIdx] = try arg.Val.kind.getTypeVal();
            } else {
                continue;
            }

            argIdx = argIdx + 1;
        }

        argsTypes.len = argIdx;

        if (def.Proc.def.data.Proc.in.len != 0) {
            var lastIdx = def.Proc.def.data.Proc.in.len - 1;

            if (std.mem.eql(u8, def.Proc.def.data.Proc.in[lastIdx], "PLUS")) {
                isVarArg = true;
            }
        }

        var functionType = llvm.functionType(kind, argsTypes.ptr, @intCast(c_uint, argsTypes.len), llvm.Bool.fromBool(isVarArg));
        var function = self.module.addFunction(try self.allocator.dupeZ(u8, name), functionType);

        var result = .{
            .value = function,
            .kind = functionType,
            .retKind = out,
            .out = null,
            .outBlock = null,
            .ctime = null,
            .name = name,
        };

        try def.Proc.impls.append(.{
            .args = try self.allocator.dupe(*Value, args),
            .value = result,
        });

        if (def.Proc.ext) {
            std.log.info("Done - {s}", .{name});

            return result;
        }

        argIdx = 0;

        for (def.Proc.def.data.Proc.in, 0..) |paramName, idx| {
            var tmpResult = try self.allocator.create(Value);
            if (args[idx].* == .ConstInt) {
                var tmpKind = try self.allocator.create(Value);

                tmpKind.* = .{
                    .Type = .{
                        .value = function.getParam(@intCast(c_uint, argIdx)).typeOf(),
                    },
                };

                tmpResult.* = .{
                    .Val = .{
                        .kind = tmpKind,
                        .value = function.getParam(@intCast(c_uint, argIdx)),
                    },
                };

                try subDefs.put(paramName, tmpResult);
            } else if (args[idx].* == .Val) {
                tmpResult.* = .{
                    .Val = .{
                        .kind = args[idx].Val.kind,
                        .value = function.getParam(@intCast(c_uint, argIdx)),
                    },
                };

                try subDefs.put(paramName, tmpResult);
            } else {
                try subDefs.put(paramName, args[idx]);
                continue;
            }
            argIdx += 1;
        }

        var oldBB = self.builder.getInsertBlock();
        var bb = self.context.appendBasicBlock(function, "entry");

        self.builder.positionBuilderAtEnd(bb);

        for (def.Proc.def.data.Proc.insts) |stmt| {
            try self.visitStatement(stmt, &result, &subDefs);
        }

        self.builder.positionBuilderAtEnd(oldBB);

        std.log.info("Done - {s}", .{name});

        return result;
    }

    pub fn run(self: *Self) interpreterError!void {
        try self.visitDefinition(null, &self.definitions, null, self.root, true, "");

        if (self.definitions.get("root")) |root| {
            if (root.Struct.subDefs.get("main")) |def| {
                var argc = try self.allocator.create(Value);
                var argv = try self.allocator.create(Value);

                argc.* = .{
                    .Val = .{
                        .value = undefined,
                        .kind = &.{
                            .Type = .{
                                .value = self.context.intType(64),
                            },
                        },
                    },
                };

                argv.* = .{
                    .Val = .{
                        .value = undefined,
                        .kind = &.{
                            .Ptr = .{
                                .kind = self.context.pointerType(0),
                                .child = &.{
                                    .Ptr = .{
                                        .kind = self.context.pointerType(0),
                                        .child = &.{
                                            .Ptr = .{
                                                .kind = self.context.pointerType(0),
                                                .child = &.{
                                                    .Type = .{
                                                        .value = self.context.intType(8),
                                                    },
                                                },
                                            },
                                        },
                                    },
                                },
                            },
                        },
                    },
                };

                var params: [2]*Value = .{ argc, argv };

                _ = try self.implement(null, def, "main", &params, &root.Struct.subDefs);

                return;
            }
        }

        return error.NoMain;
    }

    pub fn init(allocator: std.mem.Allocator, root: parser.Definition) Self {
        llvm.LLVMInitializeX86TargetInfo();
        llvm.LLVMInitializeX86Target();
        llvm.LLVMInitializeX86TargetMC();
        llvm.LLVMInitializeX86AsmParser();
        llvm.LLVMInitializeX86AsmPrinter();

        var ctx = llvm.Context.create();
        var mod = llvm.Module.createWithName("Context", ctx);
        var builder = ctx.createBuilder();

        return .{
            .strings = std.StringHashMap(*Value).init(allocator),
            .definitions = DefList.init(allocator),
            .allocator = allocator,
            .context = ctx,
            .module = mod,
            .builder = builder,
            .root = root,
        };
    }
};
