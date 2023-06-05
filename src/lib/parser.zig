const std = @import("std");
const scanner = @import("scanner.zig");

const parserError = error{
    InvalidChar,
    OutOfMemory,
    ExpectedDef,
    ExpectedColon,
    ExpectedBrace,
    ExpectedParen,
    ExpectedSemicolon,
    BadDefKind,
    InvalidCharacter,
    Overflow,
};

pub const Expression = struct {
    const ExpressionKind = enum {
        ConstInt,
        ConstString,
        Ident,
        Operation,
        Paren,
    };

    const Operation = enum {
        // binary
        BitAnd,
        BitOr,
        NotEqual,
        Greater,
        Assign,
        Access,
        IndexAccess,
        Equal,
        Less,
        Mul,
        Div,
        Mod,
        Add,
        Sub,

        // unary
        BitNot,
        Deref,
        Neg,

        // unknown
        Call,
    };

    data: union(ExpressionKind) {
        ConstInt: struct {
            value: usize,
        },
        ConstString: struct {
            value: []const u8,
        },
        Ident: struct {
            name: []const u8,
        },
        Operation: struct {
            op: Operation,
            values: []Expression,
        },
        Paren: struct {
            expr: *Expression,
        },
    },

    pub fn format(
        self: Expression,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        switch (self.data) {
            .ConstInt => |data| {
                try writer.print("#{}", .{data.value});
            },
            .ConstString => |data| {
                try writer.print("'{s}'", .{data.value});
            },
            .Ident => |data| {
                try writer.print("_{s}", .{data.name});
            },
            .Paren => |data| {
                try writer.print("[{}]", .{data.expr.*});
            },
            .Operation => |data| {
                try writer.print("{s}(", .{@tagName(data.op)});
                for (data.values, 0..) |value, idx| {
                    if (idx != 0) {
                        try writer.print(" {}", .{value});
                        continue;
                    }
                    try writer.print("{}", .{value});
                }

                _ = try writer.write(")");
            },
        }
    }
};

pub const Statement = struct {
    const StatementKind = enum {
        Expression,
        Definition,
        Return,
        While,
        If,
    };

    data: union(StatementKind) {
        Expression: Expression,
        Definition: Definition,
        Return: Expression,
        While: struct {
            check: Expression,
            body: []Statement,
        },
        If: struct {
            check: Expression,
            body: []Statement,
            bodyElse: ?[]Statement,
        },
    },

    pub fn format(
        self: Statement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        try writer.print("{s}: ", .{@tagName(self.data)});

        switch (self.data) {
            .Expression => |data| {
                try writer.print("{}", .{data});
            },
            .Definition => |data| {
                try writer.print("{}", .{data});
            },
            .Return => |data| {
                try writer.print("{}", .{data});
            },
            .If => |data| {
                try writer.print("If {} Then {any} Else {?any}", .{ data.check, data.body, data.bodyElse });
            },
            .While => |data| {
                try writer.print("While {} Do {any}", .{ data.check, data.body });
            },
        }
    }
};

pub const Definition = struct {
    const DefinitionKind = enum {
        Struct,
        Const,
        Var,
        Prop,
        Proc,
        Extern,
    };

    name: []const u8,
    data: union(DefinitionKind) {
        Struct: struct {
            subDefs: []Definition,
        },
        Const: struct {
            value: Expression,
        },
        Var: struct {
            kind: Expression,
        },
        Prop: struct {
            kind: Expression,
        },
        Proc: struct {
            in: [][]const u8,
            out: Expression,
            insts: []Statement,
            inl: bool,
        },
        Extern: struct {
            in: [][]const u8,
            out: Expression,
        },
    },

    pub fn format(
        self: Definition,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        try writer.print("{s}: _{s}", .{ @tagName(self.data), self.name });

        switch (self.data) {
            .Struct => |data| {
                for (data.subDefs) |def| {
                    try writer.print("\n" ++ fmt ++ "  {  }", .{def});
                }
            },
            .Var => |data| {
                _ = try writer.write(" of ");
                try writer.print("{}", .{data.kind});
            },
            .Const => |data| {
                _ = try writer.write(" is ");
                try writer.print("{}", .{data.value});
            },
            .Prop => |data| {
                _ = try writer.write(" of ");
                try writer.print("{}", .{data.kind});
            },
            .Proc => |data| {
                _ = try writer.write("\n" ++ fmt ++ "  In:");

                for (data.in) |in| {
                    try writer.print(" _{s}", .{in});
                }

                _ = try writer.write("\n" ++ fmt ++ "  Out: ");

                try writer.print("\n" ++ fmt ++ "    {}", .{data.out});

                for (data.insts) |inst| {
                    try writer.print("\n" ++ fmt ++ "  {}", .{inst});
                }
            },
            .Extern => |data| {
                _ = try writer.write(" of ");
                try writer.print("{}", .{data.out});
            },
        }
    }
};

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    scanner: scanner.Scanner,
    current: scanner.Scanner.Token = undefined,
    prev: scanner.Scanner.Token = undefined,

    pub fn init(scn: scanner.Scanner, alloc: std.mem.Allocator) Parser {
        return .{
            .scanner = scn,
            .allocator = alloc,
        };
    }

    pub fn advance(self: *Self) parserError!void {
        self.prev = self.current;
        self.current = try self.scanner.nextToken();
    }

    pub fn match(self: *Self, kind: scanner.Scanner.TokenType) parserError!bool {
        if (self.current.kind != kind) return false;
        try self.advance();
        return true;
    }

    const ExpressionLevel = enum {
        Assignment,
        Or,
        And,
        Equality,
        Comparison,
        Term,
        Factor,
        Unary,
        Call,
        Primary,
    };

    pub fn unary(self: *Self, current: *Expression, sublevel: ExpressionLevel, ty: scanner.Scanner.TokenType, out: Expression.Operation) parserError!bool {
        if (try self.match(ty)) {
            var values = try self.allocator.alloc(Expression, 2);
            values[0] = current.*;
            values[1] = try self.parseExpression(sublevel);

            current.* = .{
                .data = .{
                    .Operation = .{
                        .op = out,
                        .values = values,
                    },
                },
            };
            return true;
        }
        return false;
    }

    pub fn binary(self: *Self, current: *Expression, sublevel: ExpressionLevel, ty: scanner.Scanner.TokenType, out: Expression.Operation) parserError!bool {
        if (try self.match(ty)) {
            var values = try self.allocator.alloc(Expression, 2);
            values[0] = current.*;
            values[1] = try self.parseExpression(sublevel);

            current.* = .{
                .data = .{
                    .Operation = .{
                        .op = out,
                        .values = values,
                    },
                },
            };
            return true;
        }
        return false;
    }

    pub fn parseExpression(self: *Self, level: ExpressionLevel) parserError!Expression {
        if (level == .Primary) {
            var result: Expression = switch (self.current.kind) {
                .NUMBER => .{
                    .data = .{
                        .ConstInt = .{
                            .value = try std.fmt.parseInt(usize, self.current.lexeme, 0),
                        },
                    },
                },
                .STRING => .{
                    .data = .{
                        .ConstString = .{
                            .value = self.current.lexeme,
                        },
                    },
                },
                .CHAR => .{
                    .data = .{
                        .ConstInt = .{
                            .value = switch (self.current.lexeme.len) {
                                1 => self.current.lexeme[0],
                                2 => if (self.current.lexeme[0] == '\\') switch (self.current.lexeme[1]) {
                                    't' => '\t',
                                    'n' => '\n',
                                    'r' => '\r',
                                    'e' => '\x1b',
                                    else => self.current.lexeme[1],
                                } else return error.InvalidCharacter,
                                else => return error.InvalidCharacter,
                            },
                        },
                    },
                },
                .IDENTIFIER => .{
                    .data = .{
                        .Ident = .{
                            .name = self.current.lexeme,
                        },
                    },
                },
                .LEFT_PAREN => {
                    try self.advance();

                    var expr = try self.allocator.create(Expression);
                    expr.* = try self.parseExpression(.Assignment);

                    if (!try self.match(.RIGHT_PAREN)) return error.ExpectedParen;

                    return Expression{
                        .data = .{
                            .Paren = .{
                                .expr = expr,
                            },
                        },
                    };
                },
                else => {
                    std.log.info("{s}", .{@tagName(self.current.kind)});
                    return error.InvalidChar;
                },
            };

            try self.advance();

            return result;
        } else if (level == .Unary) {
            const next = @intToEnum(ExpressionLevel, @enumToInt(level) + 1);
            switch (self.current.kind) {
                .STAR => {
                    try self.advance();

                    var values = try self.allocator.alloc(Expression, 1);
                    values[0] = try self.parseExpression(next);

                    return .{
                        .data = .{
                            .Operation = .{
                                .op = .Deref,
                                .values = values,
                            },
                        },
                    };
                },
                .TILDE => {
                    try self.advance();

                    var values = try self.allocator.alloc(Expression, 1);
                    values[0] = try self.parseExpression(next);

                    return .{
                        .data = .{
                            .Operation = .{
                                .op = .BitNot,
                                .values = values,
                            },
                        },
                    };
                },
                else => {},
            }
        }

        const next = @intToEnum(ExpressionLevel, @enumToInt(level) + 1);

        var result = try self.parseExpression(next);

        switch (level) {
            .Assignment => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .EQUAL, .Assign);
                }
            },
            .And => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .AMPERSAND, .BitAnd);
                }
            },
            .Or => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .BAR, .BitOr);
                }
            },
            .Factor => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .PERCENT, .Mod);
                    added = added or try self.binary(&result, next, .SLASH, .Div);
                    added = added or try self.binary(&result, next, .STAR, .Mul);
                }
            },
            .Equality => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .EXCLAIM_EQUAL, .NotEqual);
                    added = added or try self.binary(&result, next, .EQUAL_EQUAL, .Equal);
                }
            },
            .Comparison => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .LT, .Less);
                    added = added or try self.binary(&result, next, .GT, .Greater);
                }
            },
            .Term => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .PLUS, .Add);
                    added = added or try self.binary(&result, next, .MINUS, .Sub);
                }
            },
            .Call => {
                var added = true;
                while (added) {
                    added = false;

                    if (self.current.kind == .LEFT_BRACKET) {
                        try self.advance();

                        var params = try self.allocator.alloc(Expression, 1);
                        params[0] = result;

                        while (!try self.match(.RIGHT_BRACKET)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_BRACKET)) return error.ExpectedParen;
                                break;
                            }
                        }

                        result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .IndexAccess,
                                    .values = params,
                                },
                            },
                        };

                        added = true;
                    } else if (self.current.kind == .LEFT_PAREN) {
                        try self.advance();

                        var params = try self.allocator.alloc(Expression, 1);
                        params[0] = result;

                        while (!try self.match(.RIGHT_PAREN)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedParen;
                                break;
                            }
                        }

                        result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .Call,
                                    .values = params,
                                },
                            },
                        };

                        added = true;
                    }

                    added = added or try self.binary(&result, next, .DOT, .Access);
                }
            },
            else => {},
        }

        return result;
    }

    pub fn parseStatement(self: *Self) parserError!Statement {
        switch (self.current.kind) {
            .DEF => {
                var def = try self.parseDef();

                return .{
                    .data = .{
                        .Definition = def,
                    },
                };
            },
            .RET => {
                try self.advance();

                var expr = try self.parseExpression(.Assignment);

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .data = .{
                        .Return = expr,
                    },
                };
            },
            .WHILE => {
                try self.advance();

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var check = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.alloc(Statement, 0);

                if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                while (!try self.match(.RIGHT_BRACE)) {
                    var stmt = try self.parseStatement();
                    body = try self.allocator.realloc(body, body.len + 1);
                    body[body.len - 1] = stmt;
                }

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .data = .{
                        .While = .{
                            .check = check,
                            .body = body,
                        },
                    },
                };
            },
            .IF => {
                try self.advance();

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var check = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.alloc(Statement, 0);

                if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                while (!try self.match(.RIGHT_BRACE)) {
                    var stmt = try self.parseStatement();
                    body = try self.allocator.realloc(body, body.len + 1);
                    body[body.len - 1] = stmt;
                }

                var bodyElse: ?[]Statement = null;

                if (try self.match(.ELSE)) {
                    bodyElse = try self.allocator.alloc(Statement, 0);

                    if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                    while (!try self.match(.RIGHT_BRACE)) {
                        var stmt = try self.parseStatement();
                        bodyElse = try self.allocator.realloc(bodyElse.?, bodyElse.?.len + 1);
                        bodyElse.?[bodyElse.?.len - 1] = stmt;
                    }
                }

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .data = .{
                        .If = .{
                            .check = check,
                            .body = body,
                            .bodyElse = bodyElse,
                        },
                    },
                };
            },
            else => {
                var expr = try self.parseExpression(.Assignment);

                if (!try self.match(.SEMI_COLON)) {
                    std.log.info("{}", .{expr});

                    return error.ExpectedSemicolon;
                }

                return .{
                    .data = .{
                        .Expression = expr,
                    },
                };
            },
        }
    }

    pub fn parseDef(self: *Self) parserError!Definition {
        if (!try self.match(.DEF)) return error.ExpectedDef;
        var name = self.current.lexeme;
        try self.advance();

        std.log.info("{}", .{self.current});

        if (!try self.match(.COLON)) return error.ExpectedColon;

        var defKind = self.current.kind;
        try self.advance();

        switch (defKind) {
            .STRUCT => {
                if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                var subDefs = try self.allocator.alloc(Definition, 0);

                while (!try self.match(.RIGHT_BRACE)) {
                    var def = try self.parseDef();
                    subDefs = try self.allocator.realloc(subDefs, subDefs.len + 1);
                    subDefs[subDefs.len - 1] = def;
                }

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Struct = .{
                            .subDefs = subDefs,
                        },
                    },
                };
            },
            .CONST => {
                var data = try self.parseExpression(.Or);

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Const = .{
                            .value = data,
                        },
                    },
                };
            },
            .PROC, .INLINE => {
                var in = try self.allocator.alloc([]const u8, 0);
                var insts = try self.allocator.alloc(Statement, 0);

                while (!try self.match(.ARROW)) {
                    in = try self.allocator.realloc(in, in.len + 1);
                    in[in.len - 1] = self.current.lexeme;
                    try self.advance();
                }

                var out = try self.parseExpression(.Or);

                if (!try self.match(.LEFT_BRACE)) return error.ExpectedSemicolon;

                while (!try self.match(.RIGHT_BRACE)) {
                    var next = try self.parseStatement();

                    insts = try self.allocator.realloc(insts, insts.len + 1);
                    insts[insts.len - 1] = next;
                }

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Proc = .{
                            .in = in,
                            .out = out,
                            .insts = insts,
                            .inl = defKind == .INLINE,
                        },
                    },
                };
            },
            .PROP => {
                var data = try self.parseExpression(.Or);

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Prop = .{
                            .kind = data,
                        },
                    },
                };
            },
            .VAR => {
                var data = try self.parseExpression(.Or);

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Var = .{
                            .kind = data,
                        },
                    },
                };
            },
            .EXTERN => {
                var in = try self.allocator.alloc([]const u8, 0);

                while (!try self.match(.ARROW)) {
                    in = try self.allocator.realloc(in, in.len + 1);
                    in[in.len - 1] = self.current.lexeme;
                    try self.advance();
                }

                var out = try self.parseExpression(.Or);

                if (!try self.match(.SEMI_COLON)) return error.ExpectedSemicolon;

                return .{
                    .name = name,
                    .data = .{
                        .Extern = .{
                            .in = in,
                            .out = out,
                        },
                    },
                };
            },
            else => {},
        }
        std.log.info("bad defKind: {}", .{defKind});
        return error.BadDefKind;
    }

    pub fn parse(self: *Self) !Definition {
        var result: Definition = .{
            .name = "root",
            .data = .{
                .Struct = .{
                    .subDefs = try self.allocator.alloc(Definition, 0),
                },
            },
        };

        try self.advance();
        while (!try self.match(.EOF)) {
            var def = try self.parseDef();
            result.data.Struct.subDefs = try self.allocator.realloc(result.data.Struct.subDefs, result.data.Struct.subDefs.len + 1);
            result.data.Struct.subDefs[result.data.Struct.subDefs.len - 1] = def;
        }

        return result;
    }
};
