const std = @import("std");
const ArrayList = std.ArrayList;
const OutOfMemory = std.mem.Allocator.Error.OutOfMemory;

const T = @import("token.zig");
const Token = T.Token;
const TokenType = T.TokenType;

const E = @import("expression.zig");
const Expr = E.Expr;
const Binary = E.Binary;
const Unary = E.Unary;
const Literal = E.Literal;
const LiteralType = E.LiteralType;
const Grouping = E.Grouping;
const Variable = E.Variable;

const S = @import("statement.zig");
const Statement = S.Statement;
const Print = S.Print;
const VarDeclaration = S.VarDeclaration;

// const D = @import("declaration.zig");
// const Declaration = D.Declaration;

pub const ParserError = error{ RightParenNotPresent, InvalidToken, MissingLeftOperandError, OutOfMemory, MissingSemicolon };

pub const Parser = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    current: usize,
    tokens: *const std.ArrayList(Token),

    pub fn init(allocator: std.mem.Allocator, tokens: *const std.ArrayList(Token)) Parser {
        return .{ .allocator = allocator, .current = 0, .tokens = tokens };
    }

    pub fn parser(self: *Self) !ArrayList(*Statement) {
        var statementList = ArrayList(*Statement).init(self.allocator);
        while (!self.isAtEnd()) {
            const stmt: *Statement = try self.statement();
            try statementList.append(stmt);
        }
        return statementList;
    }

    fn declaration(self: *Self) !*Statement {
        if (self.match_one(.VAR)) {
            return self.varDeclaration() catch |err| {
                self.synchronize();
                return err;
            };
        }

        return self.statement();
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn varDeclaration(self: *Self) !*Statement {
        const name: Token = self.advance();
        const initializer: *Expr = undefined;
        if (self.match_one(.EQUAL)) {
            initializer = try self.expression();
        } else {
            initializer = Literal.create(self.allocator, Literal{ .nil = void });
        }

        _ = self.advance();

        return try VarDeclaration.create(self.allocator, name, initializer);
    }

    fn statement(self: *Self) ParserError!*Statement {
        if (self.match_one(.PRINT)) {
            return self.printStatement();
        }
        const stmt = self.expressionStatement();
        return stmt;
    }

    fn printStatement(self: *Self) ParserError!*Statement {
        const expr: *Expr = try self.expression();
        errdefer expr.deinit(self.allocator);

        if (!self.check(.SEMICOLON)) {
            return ParserError.MissingSemicolon;
        }

        _ = self.advance();
        const stmt: *Statement = try Print.create(self.allocator, expr);
        return stmt;
    }

    fn expressionStatement(self: *Self) ParserError!*Statement {
        const expr: *Expr = try self.expression();
        errdefer expr.deinit(self.allocator);

        if (!self.check(.SEMICOLON)) {
            return ParserError.MissingSemicolon;
        }
        _ = self.advance();
        const stmt: *Statement = try self.allocator.create(Statement);
        stmt.* = .{ .expression = expr };
        return stmt;
    }

    fn expression(self: *Self) ParserError!*Expr {
        const result = try self.comma();
        return result;
    }

    fn comma(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }
        var expr: *Expr = try self.equality();
        const matches = [_]TokenType{.COMMA};

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.equality();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }

        return expr;
    }

    fn equality(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.comparison();

        const matches = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right = try self.comparison();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn comparison(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.term();

        const matches = [_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.term();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn term(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.factor();

        const matches = [_]TokenType{ .PLUS, .MINUS };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.factor();
            expr = try Binary.create(self.allocator, expr, operator, right);
            errdefer expr.deinit(self.allocator);
        }
        return expr;
    }

    fn factor(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        var expr: *Expr = try self.unary();
        errdefer expr.deinit(self.allocator);
        const matches = [_]TokenType{ .SLASH, .STAR };

        while (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();
            expr = try Binary.create(self.allocator, expr, operator, right);
        }
        return expr;
    }

    fn unary(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        const matches = [_]TokenType{ .BANG, .MINUS };

        if (self.match(&matches)) {
            const operator: Token = self.previous();
            const right: *Expr = try self.unary();

            const expr = try Unary.create(self.allocator, operator, right);

            return expr;
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParserError!*Expr {
        if (self.validate_left_operand()) {
            return ParserError.MissingLeftOperandError;
        }

        switch (self.peek().kind) {
            .FALSE => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .boolean = false });
            },
            .TRUE => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .boolean = true });
            },
            .NIL => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .nil = void{} });
            },
            .NUMBER => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .number = self.previous().literal.?.number });
            },
            .STRING => {
                _ = self.advance();
                return try Literal.create(self.allocator, Literal{ .string = self.previous().literal.?.string });
            },

            // Handle the parens
            .LEFT_PAREN => {
                _ = self.advance();
                const expr: *Expr = try self.expression();

                // Check if the right paren is present
                switch (self.check(.RIGHT_PAREN)) {
                    true => _ = self.advance(),
                    false => std.debug.panic("[Line {d}] Error at {any}: expect ')' after expression.", .{ self.peek().line, self.peek() }),
                }

                return try Grouping.create(self.allocator, expr);
            },
            .IDENTIFIER => {
                _ = self.advance();
                const name = self.advance();
                return try Variable.create(self.allocator, name);
            },
            else => |invalidToken| std.debug.panic("Error: {any} - Current Token: {any}\n", .{ ParserError.InvalidToken, invalidToken }),
        }
    }

    fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().kind == .SEMICOLON) {
                return;
            }

            switch (self.peek().kind) {
                .CLASS => return,
                .FUN => return,
                .VAR => return,
                .FOR => return,
                .IF => return,
                .WHILE => return,
                .PRINT => return,
                .RETURN => return,
            }

            self.advance();
        }
    }

    fn validate_left_operand(self: *Self) bool {
        // MINUS is removed from the matches because it doesn't need to have a left hand operator. It can be used with a number like -5, and clearly it doesn't have a left hand operator.
        const matches = [_]TokenType{
            .EQUAL_EQUAL,
            .BANG_EQUAL,
            .LESS,
            .LESS_EQUAL,
            .GREATER,
            .GREATER_EQUAL,
            .PLUS,
            .STAR,
            .COMMA,
        };

        return self.match(&matches);
    }

    fn match_one(self: *Self, tokenType: TokenType) bool {
        if (self.check(tokenType)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    fn match(self: *Self, tokenTypeList: []const TokenType) bool {
        for (tokenTypeList) |tokenType| {
            if (self.check(tokenType)) {
                _ = self.advance();
                return true;
            }
        }

        return false;
    }

    fn check(self: *Self, tokenType: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }

        return self.peek().kind == tokenType;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }

        return self.previous();
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().kind == TokenType.EOF;
    }

    fn peek(self: Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: Self) Token {
        return self.tokens.items[self.current - 1];
    }
};

// expression     → comma
// comma          → equality ( (",") equality)*
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
//                | IDENTIFIER ;

// Add error productions to handle each binary operator appearing without a left-hand operand.
// In other words, detect a binary operator appearing at the beginning of an expression.
// Report that as an error, but also parse and discard a right-hand operand with the appropriate precedence.
