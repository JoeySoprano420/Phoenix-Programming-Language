/*
Phoenix Language Transpiler / Proto Compiler
Single-file prototype: Lexer -> Parser -> Semantic -> IR -> x86-64 NASM emission
NOTE: Adapt grammar & semantics to match your official syntax_sheet.md and semantic_sheet.md.

Current Supported Subset (extend to full spec):
  - Primitive types: int, bool
  - Literals: decimal integers, true, false
  - Statements: let (immutable), var (mutable), assignment, if/else, while, return, print(...)
  - Functions: fn name(params...) : type { ... }
  - Expressions: + - * / % unary - !, comparison (== != < <= > >=), logical && || (short-circuit simulated), grouping, calls.
  - Scoping: block-level
  - Code generation: Produces NASM AT&T disabled, Intel syntax (default NASM) for Windows x64 System V subset simulated.
    (Adapt calling convention if targeting Windows PE + MS x64: RCX, RDX, R8, R9 for first 4 args.)

DISCLAIMER:
  This is a scaffold; integrate full grammar & semantics from your official sheets.
  Not all error cases handled. Expand for full language coverage.
*/

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <optional>
#include <cctype>
#include <algorithm>
#include <stdexcept>
#include <memory>
#include <filesystem>
#include <cstdio>

enum class TokenKind {
    End, Identifier, Integer, KwFn, KwLet, KwVar, KwReturn,
    KwIf, KwElse, KwWhile, KwTrue, KwFalse, KwPrint,
    LParen, RParen, LBrace, RBrace, Comma, Colon, Semicolon, Arrow,
    Plus, Minus, Star, Slash, Percent,
    Assign, Bang, BangEq, EqEq, Lt, LtEq, Gt, GtEq,
    AndAnd, OrOr
};

struct Token {
    TokenKind kind;
    std::string lexeme;
    int line;
};

class Lexer {
    std::string src;
    size_t pos = 0;
    int line = 1;
public:
    explicit Lexer(std::string s) : src(std::move(s)) {}
    std::vector<Token> lex() {
        std::vector<Token> tokens;
        while (true) {
            skipWhitespaceAndComments();
            if (eof()) { tokens.push_back({ TokenKind::End,"",line }); break; }
            char c = peek();
            if (isalpha(c) || c == '_') {
                tokens.push_back(identifier());
            }
            else if (isdigit(c)) {
                tokens.push_back(number());
            }
            else {
                switch (c) {
                case '(': tokens.push_back(simple(TokenKind::LParen)); break;
                case ')': tokens.push_back(simple(TokenKind::RParen)); break;
                case '{': tokens.push_back(simple(TokenKind::LBrace)); break;
                case '}': tokens.push_back(simple(TokenKind::RBrace)); break;
                case ',': tokens.push_back(simple(TokenKind::Comma)); break;
                case ':': tokens.push_back(simple(TokenKind::Colon)); break;
                case ';': tokens.push_back(simple(TokenKind::Semicolon)); break;
                case '+': tokens.push_back(simple(TokenKind::Plus)); break;
                case '-':
                    if (match('>')) tokens.push_back(Token{ TokenKind::Arrow,"->",line });
                    else tokens.push_back(simple(TokenKind::Minus));
                    break;
                case '*': tokens.push_back(simple(TokenKind::Star)); break;
                case '%': tokens.push_back(simple(TokenKind::Percent)); break;
                case '/':
                    tokens.push_back(simple(TokenKind::Slash));
                    break;
                case '=':
                    if (match('=')) tokens.push_back(Token{ TokenKind::EqEq,"==",line });
                    else tokens.push_back(simple(TokenKind::Assign));
                    break;
                case '!':
                    if (match('=')) tokens.push_back(Token{ TokenKind::BangEq,"!=",line });
                    else tokens.push_back(simple(TokenKind::Bang));
                    break;
                case '<':
                    if (match('=')) tokens.push_back(Token{ TokenKind::LtEq,"<=",line });
                    else tokens.push_back(simple(TokenKind::Lt));
                    break;
                case '>':
                    if (match('=')) tokens.push_back(Token{ TokenKind::GtEq,">=",line });
                    else tokens.push_back(simple(TokenKind::Gt));
                    break;
                case '&':
                    require('&');
                    tokens.push_back(Token{ TokenKind::AndAnd,"&&",line });
                    break;
                case '|':
                    require('|');
                    tokens.push_back(Token{ TokenKind::OrOr,"||",line });
                    break;
                default:
                    throw std::runtime_error("Unexpected char at line " + std::to_string(line) + ": " + c);
                }
            }
        }
        return tokens;
    }
private:
    bool eof() const { return pos >= src.size(); }
    char peek() const { return src[pos]; }
    char advance() { return src[pos++]; }
    bool match(char expected) {
        if (eof() || src[pos] != expected) return false;
        pos++; return true;
    }
    void require(char expected) {
        if (!match(expected)) throw std::runtime_error("Expected char missing");
    }
    void skipWhitespaceAndComments() {
        while (!eof()) {
            char c = peek();
            if (c == ' ' || c == '\r' || c == '\t') { advance(); }
            else if (c == '\n') { advance(); line++; }
            else if (c == '/' && pos + 1 < src.size() && src[pos + 1] == '/') {
                while (!eof() && peek() != '\n') advance();
            }
            else {
                break;
            }
        }
    }
    Token simple(TokenKind kind) {
        std::string l(1, advance());
        return Token{ kind,l,line };
    }
    Token identifier() {
        size_t start = pos;
        while (!eof() && (isalnum(peek()) || peek() == '_')) advance();
        std::string text = src.substr(start, pos - start);
        TokenKind kind = TokenKind::Identifier;
        if (text == "fn") kind = TokenKind::KwFn;
        else if (text == "let") kind = TokenKind::KwLet;
        else if (text == "var") kind = TokenKind::KwVar;
        else if (text == "return") kind = TokenKind::KwReturn;
        else if (text == "if") kind = TokenKind::KwIf;
        else if (text == "else") kind = TokenKind::KwElse;
        else if (text == "while") kind = TokenKind::KwWhile;
        else if (text == "true") kind = TokenKind::KwTrue;
        else if (text == "false") kind = TokenKind::KwFalse;
        else if (text == "print") kind = TokenKind::KwPrint;
        return Token{ kind,text,line };
    }
    Token number() {
        size_t start = pos;
        while (!eof() && isdigit(peek())) advance();
        return Token{ TokenKind::Integer, src.substr(start,pos - start), line };
    }
};

// AST
struct Expr;
struct Stmt;
using ExprPtr = std::shared_ptr<Expr>;
using StmtPtr = std::shared_ptr<Stmt>;

enum class TypeKind { Int, Bool, Void, Unknown };

struct Type {
    TypeKind kind;
};

struct Expr {
    int line = 0;
    virtual ~Expr() = default;
};

struct ExprLiteral : Expr {
    int value; bool isBool = false;
};

struct ExprVar : Expr {
    std::string name;
};

struct ExprUnary : Expr {
    TokenKind op;
    ExprPtr rhs;
};

struct ExprBinary : Expr {
    ExprPtr left;
    TokenKind op;
    ExprPtr right;
};

struct ExprCall : Expr {
    std::string callee;
    std::vector<ExprPtr> args;
};

struct Stmt {
    int line = 0;
    virtual ~Stmt() = default;
};

struct StmtVarDecl : Stmt {
    bool mutableVar = false;
    std::string name;
    Type type;
    ExprPtr init;
};

struct StmtExpr : Stmt {
    ExprPtr expr;
};

struct StmtAssign : Stmt {
    std::string name;
    ExprPtr value;
};

struct StmtIf : Stmt {
    ExprPtr cond;
    std::vector<StmtPtr> thenBranch;
    std::vector<StmtPtr> elseBranch;
};

struct StmtWhile : Stmt {
    ExprPtr cond;
    std::vector<StmtPtr> body;
};

struct StmtReturn : Stmt {
    ExprPtr value; // may be null
};

struct StmtPrint : Stmt {
    ExprPtr value;
};

struct Function {
    std::string name;
    std::vector<std::pair<std::string, Type>> params;
    Type returnType;
    std::vector<StmtPtr> body;
    int line = 0;
};

struct Program {
    std::vector<std::shared_ptr<Function>> functions;
};

// Simple Parser
class Parser {
    const std::vector<Token>& tokens;
    size_t idx = 0;
public:
    explicit Parser(const std::vector<Token>& t) : tokens(t) {}
    Program parse() {
        Program p;
        while (!match(TokenKind::End)) {
            p.functions.push_back(parseFunction());
        }
        return p;
    }
private:
    bool check(TokenKind k) const {
        return tokens[idx].kind == k;
    }
    const Token& advance() {
        return tokens[idx++];
    }
    bool match(TokenKind k) {
        if (check(k)) { advance(); return true; }
        return false;
    }
    const Token& expect(TokenKind k, const char* msg) {
        if (!check(k)) throw std::runtime_error(std::string(msg) + " at line " + std::to_string(tokens[idx].line));
        return advance();
    }
    std::shared_ptr<Function> parseFunction() {
        expect(TokenKind::KwFn, "Expected 'fn'");
        auto nameTok = expect(TokenKind::Identifier, "Expected function name");
        auto fn = std::make_shared<Function>();
        fn->name = nameTok.lexeme;
        fn->line = nameTok.line;
        expect(TokenKind::LParen, "Expected '('");
        if (!check(TokenKind::RParen)) {
            do {
                auto paramName = expect(TokenKind::Identifier, "Expected param name");
                Type t = parseTypeAnnotationOptional();
                fn->params.push_back({ paramName.lexeme, t });
            } while (match(TokenKind::Comma));
        }
        expect(TokenKind::RParen, "Expected ')'");
        fn->returnType = parseReturnTypeOptional();
        expect(TokenKind::LBrace, "Expected '{'");
        fn->body = parseBlock();
        return fn;
    }
    Type parseTypeAnnotationOptional() {
        // Optional : type  (Currently only : int | bool)
        if (match(TokenKind::Colon)) {
            auto id = expect(TokenKind::Identifier, "Expected type name");
            if (id.lexeme == "int") return { TypeKind::Int };
            if (id.lexeme == "bool") return { TypeKind::Bool };
            throw std::runtime_error("Unknown type " + id.lexeme + " line " + std::to_string(id.line));
        }
        return { TypeKind::Unknown };
    }
    Type parseReturnTypeOptional() {
        if (match(TokenKind::Arrow)) {
            auto id = expect(TokenKind::Identifier, "Expected return type");
            if (id.lexeme == "int") return { TypeKind::Int };
            if (id.lexeme == "bool") return { TypeKind::Bool };
            if (id.lexeme == "void") return { TypeKind::Void };
            throw std::runtime_error("Unknown return type: " + id.lexeme);
        }
        return { TypeKind::Void };
    }
    std::vector<StmtPtr> parseBlock() {
        std::vector<StmtPtr> stmts;
        while (!check(TokenKind::RBrace)) {
            stmts.push_back(parseStatement());
        }
        expect(TokenKind::RBrace, "Expected '}'");
        return stmts;
    }
    StmtPtr parseStatement() {
        if (match(TokenKind::KwLet) || match(TokenKind::KwVar)) {
            bool mut = tokens[idx - 1].kind == TokenKind::KwVar;
            auto nameTok = expect(TokenKind::Identifier, "Expected variable name");
            Type t = parseTypeAnnotationOptional();
            expect(TokenKind::Assign, "Expected '=' in declaration");
            auto init = parseExpression();
            expect(TokenKind::Semicolon, "Missing ';'");
            auto d = std::make_shared<StmtVarDecl>();
            d->mutableVar = mut; d->name = nameTok.lexeme; d->type = t; d->init = init; d->line = nameTok.line;
            return d;
        }
        if (match(TokenKind::KwIf)) {
            auto stmt = std::make_shared<StmtIf>();
            stmt->line = tokens[idx - 1].line;
            expect(TokenKind::LParen, "Expected '('");
            stmt->cond = parseExpression();
            expect(TokenKind::RParen, "Expected ')'");
            expect(TokenKind::LBrace, "Expected '{'");
            stmt->thenBranch = parseBlock();
            if (match(TokenKind::KwElse)) {
                expect(TokenKind::LBrace, "Expected '{' after else");
                stmt->elseBranch = parseBlock();
            }
            return stmt;
        }
        if (match(TokenKind::KwWhile)) {
            auto stmt = std::make_shared<StmtWhile>();
            stmt->line = tokens[idx - 1].line;
            expect(TokenKind::LParen, "Expected '('");
            stmt->cond = parseExpression();
            expect(TokenKind::RParen, "Expected ')'");
            expect(TokenKind::LBrace, "Expected '{'");
            stmt->body = parseBlock();
            return stmt;
        }
        if (match(TokenKind::KwReturn)) {
            auto r = std::make_shared<StmtReturn>();
            r->line = tokens[idx - 1].line;
            if (!check(TokenKind::Semicolon)) {
                r->value = parseExpression();
            }
            expect(TokenKind::Semicolon, "Missing ';'");
            return r;
        }
        if (match(TokenKind::KwPrint)) {
            auto p = std::make_shared<StmtPrint>();
            p->line = tokens[idx - 1].line;
            expect(TokenKind::LParen, "Expected '('");
            p->value = parseExpression();
            expect(TokenKind::RParen, "Expected ')'");
            expect(TokenKind::Semicolon, "Missing ';'");
            return p;
        }
        // Assignment or expression?
        if (check(TokenKind::Identifier) && tokens[idx + 1].kind == TokenKind::Assign) {
            auto nameTok = advance();
            advance(); // =
            auto a = std::make_shared<StmtAssign>();
            a->name = nameTok.lexeme;
            a->value = parseExpression();
            a->line = nameTok.line;
            expect(TokenKind::Semicolon, "Missing ';'");
            return a;
        }
        auto e = parseExpression();
        expect(TokenKind::Semicolon, "Missing ';'");
        auto se = std::make_shared<StmtExpr>();
        se->expr = e;
        se->line = e->line;
        return se;
    }
    // Pratt or precedence-based expression parsing:
    ExprPtr parseExpression() { return parseLogicalOr(); }
    ExprPtr parseLogicalOr() {
        auto expr = parseLogicalAnd();
        while (match(TokenKind::OrOr)) {
            auto op = tokens[idx - 1];
            auto rhs = parseLogicalAnd();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseLogicalAnd() {
        auto expr = parseEquality();
        while (match(TokenKind::AndAnd)) {
            auto op = tokens[idx - 1];
            auto rhs = parseEquality();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseEquality() {
        auto expr = parseComparison();
        while (check(TokenKind::EqEq) || check(TokenKind::BangEq)) {
            auto op = advance();
            auto rhs = parseComparison();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseComparison() {
        auto expr = parseTerm();
        while (check(TokenKind::Lt) || check(TokenKind::LtEq) || check(TokenKind::Gt) || check(TokenKind::GtEq)) {
            auto op = advance();
            auto rhs = parseTerm();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseTerm() {
        auto expr = parseFactor();
        while (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            auto op = advance();
            auto rhs = parseFactor();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseFactor() {
        auto expr = parseUnary();
        while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent)) {
            auto op = advance();
            auto rhs = parseUnary();
            auto b = std::make_shared<ExprBinary>();
            b->left = expr; b->op = op.kind; b->right = rhs; b->line = op.line;
            expr = b;
        }
        return expr;
    }
    ExprPtr parseUnary() {
        if (check(TokenKind::Bang) || check(TokenKind::Minus)) {
            auto op = advance();
            auto rhs = parseUnary();
            auto u = std::make_shared<ExprUnary>();
            u->op = op.kind; u->rhs = rhs; u->line = op.line;
            return u;
        }
        return parsePrimary();
    }
    ExprPtr parsePrimary() {
        if (match(TokenKind::Integer)) {
            auto lit = std::make_shared<ExprLiteral>();
            lit->value = std::stoi(tokens[idx - 1].lexeme);
            lit->line = tokens[idx - 1].line;
            return lit;
        }
        if (match(TokenKind::KwTrue)) {
            auto lit = std::make_shared<ExprLiteral>();
            lit->value = 1; lit->isBool = true; lit->line = tokens[idx - 1].line;
            return lit;
        }
        if (match(TokenKind::KwFalse)) {
            auto lit = std::make_shared<ExprLiteral>();
            lit->value = 0; lit->isBool = true; lit->line = tokens[idx - 1].line;
            return lit;
        }
        if (match(TokenKind::Identifier)) {
            auto idTok = tokens[idx - 1];
            if (match(TokenKind::LParen)) {
                auto call = std::make_shared<ExprCall>();
                call->callee = idTok.lexeme;
                call->line = idTok.line;
                if (!check(TokenKind::RParen)) {
                    do {
                        call->args.push_back(parseExpression());
                    } while (match(TokenKind::Comma));
                }
                expect(TokenKind::RParen, "Expected ')'");
                return call;
            }
            auto v = std::make_shared<ExprVar>();
            v->name = idTok.lexeme; v->line = idTok.line;
            return v;
        }
        if (match(TokenKind::LParen)) {
            auto e = parseExpression();
            expect(TokenKind::RParen, "Expected ')'");
            return e;
        }
        throw std::runtime_error("Unexpected token in expression at line " + std::to_string(tokens[idx].line));
    }
};

// Semantic structures
struct VarInfo {
    Type type;
    bool mutableVar;
    int stackOffset; // negative offset from RBP
};

struct FuncInfo {
    Type returnType;
    std::vector<Type> paramTypes;
};

// IR Instruction (very high-level)
enum class IROp {
    PushConst, LoadVar, StoreVar,
    Add, Sub, Mul, Div, Mod,
    CmpEq, CmpNe, CmpLt, CmpLe, CmpGt, CmpGe,
    LogicAnd, LogicOr, LogicNot, Neg,
    Jump, JumpIfFalse,
    Label,
    Call, Ret,
    Pop,
    Print
};

struct IRInstr {
    IROp op;
    int a = 0;
    std::string s;
};

// Code Generator
class CodeGen {
public:
    std::vector<IRInstr> ir;
private:
    int labelCounter = 0;
    std::vector<std::unordered_map<std::string, VarInfo>> scopes;
    std::unordered_map<std::string, FuncInfo> functions;
    int currentStackOffset = 0; // grows negatively
    int tempCounter = 0;
public:
    void generate(const Program& prog) {
        // Register function signatures first
        for (auto& f : prog.functions) {
            FuncInfo info;
            info.returnType = f->returnType;
            for (auto& p : f->params) info.paramTypes.push_back(p.second);
            functions[f->name] = info;
        }
        for (auto& f : prog.functions) {
            startFunction(f->name);
            // Parameter stack home allocation (simplified; not actual ABI)
            enterScope();
            for (auto& p : f->params) {
                VarInfo vi;
                vi.type = p.second.kind == TypeKind::Unknown ? Type{ TypeKind::Int } : p.second;
                vi.mutableVar = true;
                currentStackOffset -= 8;
                vi.stackOffset = currentStackOffset;
                scopes.back()[p.first] = vi;
            }
            for (auto& s : f->body) emitStmt(s);
            // Implicit return
            if (f->returnType.kind == TypeKind::Void) {
                ir.push_back({ IROp::Ret });
            }
            exitScope();
        }
    }
private:
    void enterScope() { scopes.emplace_back(); }
    void exitScope() {
        scopes.pop_back();
    }
    VarInfo* lookupVar(const std::string& name) {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto f = it->find(name);
            if (f != it->end()) return &f->second;
        }
        return nullptr;
    }
    void startFunction(const std::string& name) {
        ir.push_back({ IROp::Label,0,name });
        currentStackOffset = 0;
        scopes.clear();
    }
    std::string newLabel(const std::string& base) {
        return base + "_" + std::to_string(labelCounter++);
    }
    void emitExpr(const ExprPtr& e) {
        if (auto lit = std::dynamic_pointer_cast<ExprLiteral>(e)) {
            ir.push_back({ IROp::PushConst,lit->value });
        }
        else if (auto var = std::dynamic_pointer_cast<ExprVar>(e)) {
            auto vi = lookupVar(var->name);
            if (!vi) throw std::runtime_error("Undefined variable " + var->name);
            ir.push_back({ IROp::LoadVar,vi->stackOffset });
        }
        else if (auto un = std::dynamic_pointer_cast<ExprUnary>(e)) {
            emitExpr(un->rhs);
            if (un->op == TokenKind::Bang) ir.push_back({ IROp::LogicNot });
            else if (un->op == TokenKind::Minus) ir.push_back({ IROp::Neg });
        }
        else if (auto bin = std::dynamic_pointer_cast<ExprBinary>(e)) {
            if (bin->op == TokenKind::AndAnd) {
                // short-circuit
                std::string endL = newLabel("and_end");
                std::string falseL = newLabel("and_false");
                emitExpr(bin->left);
                ir.push_back({ IROp::JumpIfFalse,0,falseL });
                emitExpr(bin->right);
                ir.push_back({ IROp::Jump,0,endL });
                ir.push_back({ IROp::Label,0,falseL });
                ir.push_back({ IROp::PushConst,0 });
                ir.push_back({ IROp::Label,0,endL });
                return;
            }
            else if (bin->op == TokenKind::OrOr) {
                std::string endL = newLabel("or_end");
                std::string trueL = newLabel("or_true");
                emitExpr(bin->left);
                // If left true jump produce 1
                // Duplicate top by re-load; simplified approach: evaluate left, if false evaluate right else skip
                ir.push_back({ IROp::JumpIfFalse,0,trueL }); // Actually branch on false; we invert by rewriting logic
                // left was true -> keep it (1)
                ir.push_back({ IROp::Jump,0,endL });
                ir.push_back({ IROp::Label,0,trueL });
                // left false -> pop? For simplicity assume booleans 0/1, we will evaluate right and ignore left (imperfection)
                emitExpr(bin->right);
                ir.push_back({ IROp::Label,0,endL });
                return;
            }
            emitExpr(bin->left);
            emitExpr(bin->right);
            switch (bin->op) {
            case TokenKind::Plus: ir.push_back({ IROp::Add }); break;
            case TokenKind::Minus: ir.push_back({ IROp::Sub }); break;
            case TokenKind::Star: ir.push_back({ IROp::Mul }); break;
            case TokenKind::Slash: ir.push_back({ IROp::Div }); break;
            case TokenKind::Percent: ir.push_back({ IROp::Mod }); break;
            case TokenKind::EqEq: ir.push_back({ IROp::CmpEq }); break;
            case TokenKind::BangEq: ir.push_back({ IROp::CmpNe }); break;
            case TokenKind::Lt: ir.push_back({ IROp::CmpLt }); break;
            case TokenKind::LtEq: ir.push_back({ IROp::CmpLe }); break;
            case TokenKind::Gt: ir.push_back({ IROp::CmpGt }); break;
            case TokenKind::GtEq: ir.push_back({ IROp::CmpGe }); break;
            default: throw std::runtime_error("Unsupported binary operator");
            }
        }
        else if (auto call = std::dynamic_pointer_cast<ExprCall>(e)) {
            for (auto& a : call->args) emitExpr(a);
            ir.push_back({ IROp::Call,(int)call->args.size(),call->callee });
        }
        else {
            throw std::runtime_error("Unknown expression node");
        }
    }
    void emitStmt(const StmtPtr& s) {
        if (auto d = std::dynamic_pointer_cast<StmtVarDecl>(s)) {
            emitExpr(d->init);
            // Allocate stack slot
            if (scopes.empty()) enterScope();
            VarInfo vi;
            vi.type = d->type.kind == TypeKind::Unknown ? Type{ TypeKind::Int } : d->type;
            vi.mutableVar = d->mutableVar;
            currentStackOffset -= 8;
            vi.stackOffset = currentStackOffset;
            scopes.back()[d->name] = vi;
            ir.push_back({ IROp::StoreVar,vi.stackOffset });
        }
        else if (auto a = std::dynamic_pointer_cast<StmtAssign>(s)) {
            auto vi = lookupVar(a->name);
            if (!vi) throw std::runtime_error("Assign unknown var");
            if (!vi->mutableVar) throw std::runtime_error("Cannot assign immutable var");
            emitExpr(a->value);
            ir.push_back({ IROp::StoreVar,vi->stackOffset });
        }
        else if (auto r = std::dynamic_pointer_cast<StmtReturn>(s)) {
            if (r->value) emitExpr(r->value);
            ir.push_back({ IROp::Ret });
        }
        else if (auto w = std::dynamic_pointer_cast<StmtWhile>(s)) {
            std::string startL = newLabel("while_start");
            std::string endL = newLabel("while_end");
            ir.push_back({ IROp::Label,0,startL });
            emitExpr(w->cond);
            ir.push_back({ IROp::JumpIfFalse,0,endL });
            enterScope();
            for (auto& st : w->body) emitStmt(st);
            exitScope();
            ir.push_back({ IROp::Jump,0,startL });
            ir.push_back({ IROp::Label,0,endL });
        }
        else if (auto i = std::dynamic_pointer_cast<StmtIf>(s)) {
            std::string elseL = newLabel("if_else");
            std::string endL = newLabel("if_end");
            emitExpr(i->cond);
            ir.push_back({ IROp::JumpIfFalse,0,elseL });
            enterScope();
            for (auto& st : i->thenBranch) emitStmt(st);
            exitScope();
            ir.push_back({ IROp::Jump,0,endL });
            ir.push_back({ IROp::Label,0,elseL });
            if (!i->elseBranch.empty()) {
                enterScope();
                for (auto& st : i->elseBranch) emitStmt(st);
                exitScope();
            }
            ir.push_back({ IROp::Label,0,endL });
        }
        else if (auto p = std::dynamic_pointer_cast<StmtPrint>(s)) {
            emitExpr(p->value);
            ir.push_back({ IROp::Print });
        }
        else if (auto e = std::dynamic_pointer_cast<StmtExpr>(s)) {
            emitExpr(e->expr);
            ir.push_back({ IROp::Pop });
        }
        else {
            throw std::runtime_error("Unknown statement kind");
        }
    }
};

// Assembly Emitter (NASM x86-64)
// Very simplified stack machine to register lowering: use RAX/RBX as temporaries
class AsmEmitter {
public:
    std::string emit(const std::vector<IRInstr>& ir, const std::string& originalSource) {
        std::ostringstream out;
        out << ";; Generated NASM Assembly (Phoenix -> x86-64)\n";
        out << "default rel\n";
        out << "section .data\n";
        out << "fmt_int db \"%d\",10,0\n";
        out << "section .text\n";
        out << "global main\n";
        out << "extern printf\n\n";
        int stackFrameSize = 1024; // static frame reservation (simplified)
        std::unordered_map<std::string, int> labelIds;
        // Pre-scan labels
        for (auto& ins : ir) if (ins.op == IROp::Label) labelIds[ins.s]++;
        size_t ip = 0;
        out << "main:\n";
        out << "    push rbp\n";
        out << "    mov rbp, rsp\n";
        out << "    sub rsp, " << stackFrameSize << "\n";
        // We'll maintain a simulated operand stack at (rbp - 8*stackTop)
        int virtualTop = 0;
        auto popTo = [&](const std::string& reg) {
            virtualTop--;
            out << "    mov " << reg << ", [rbp-" << (8 * (virtualTop + 1)) << "]\n";
            };
        auto pushReg = [&](const std::string& reg) {
            out << "    mov [rbp-" << (8 * (virtualTop + 1)) << "], " << reg << "\n";
            virtualTop++;
            };
        std::unordered_map<std::string, int> labelToAsmId;
        int labelSeq = 0;
        // First assign each label an assembly name
        for (auto& ins : ir) {
            if (ins.op == IROp::Label) {
                if (!labelToAsmId.count(ins.s))
                    labelToAsmId[ins.s] = labelSeq++;
            }
        }
        // Emit instructions
        for (auto& ins : ir) {
            switch (ins.op) {
            case IROp::Label:
                out << "L" << labelToAsmId[ins.s] << ":\n";
                break;
            case IROp::PushConst:
                out << "    mov rax, " << ins.a << "    ; push const\n";
                pushReg("rax");
                break;
            case IROp::LoadVar:
                out << "    mov rax, [rbp" << (ins.a < 0 ? std::to_string(ins.a) : ("+" + std::to_string(ins.a))) << "] ; load var\n";
                pushReg("rax");
                break;
            case IROp::StoreVar:
                popTo("rax");
                out << "    mov [rbp" << (ins.a < 0 ? std::to_string(ins.a) : ("+" + std::to_string(ins.a))) << "], rax ; store var\n";
                break;
            case IROp::Add:
            case IROp::Sub:
            case IROp::Mul:
            case IROp::Div:
            case IROp::Mod: {
                popTo("rbx");
                popTo("rax");
                if (ins.op == IROp::Add) out << "    add rax, rbx\n";
                else if (ins.op == IROp::Sub) out << "    sub rax, rbx\n";
                else if (ins.op == IROp::Mul) out << "    imul rbx\n";
                else if (ins.op == IROp::Div || ins.op == IROp::Mod) {
                    out << "    cqo\n";
                    out << "    idiv rbx\n";
                    if (ins.op == IROp::Mod) out << "    mov rax, rdx\n";
                }
                pushReg("rax");
                break;
            }
            case IROp::Neg:
                popTo("rax");
                out << "    neg rax\n";
                pushReg("rax");
                break;
            case IROp::LogicNot:
                popTo("rax");
                out << "    cmp rax, 0\n";
                out << "    sete al\n";
                out << "    movzx rax, al\n";
                pushReg("rax");
                break;
            case IROp::CmpEq: case IROp::CmpNe: case IROp::CmpLt: case IROp::CmpLe: case IROp::CmpGt: case IROp::CmpGe: {
                popTo("rbx");
                popTo("rax");
                out << "    cmp rax, rbx\n";
                const char* setInstr = "sete";
                switch (ins.op) {
                case IROp::CmpEq: setInstr = "sete"; break;
                case IROp::CmpNe: setInstr = "setne"; break;
                case IROp::CmpLt: setInstr = "setl"; break;
                case IROp::CmpLe: setInstr = "setle"; break;
                case IROp::CmpGt: setInstr = "setg"; break;
                case IROp::CmpGe: setInstr = "setge"; break;
                default: break;
                }
                out << "    " << setInstr << " al\n";
                out << "    movzx rax, al\n";
                pushReg("rax");
                break;
            }
            case IROp::Jump: {
                out << "    jmp L" << labelToAsmId[ins.s] << "\n";
                break;
            }
            case IROp::JumpIfFalse: {
                popTo("rax");
                out << "    cmp rax, 0\n";
                out << "    je L" << labelToAsmId[ins.s] << "\n";
                break;
            }
            case IROp::Call: {
                // Simplified: Evaluate args already on stack (top = last). Move them into registers (Windows x64 RCX,RDX,R8,R9)
                static const char* winRegs[4] = { "rcx","rdx","r8","r9" };
                for (int i = ins.a - 1; i >= 0; --i) {
                    // Arg i is at virtualTop - (ins.a - i)
                    int slotIndex = virtualTop - (ins.a - i);
                    out << "    mov " << winRegs[i] << ", [rbp-" << (8 * (slotIndex)) << "]\n";
                }
                // Pop args (virtual only)
                virtualTop -= ins.a;
                out << "    call " << mangleUserFunction(ins.s) << "\n";
                pushReg("rax");
                break;
            }
            case IROp::Ret: {
                popTo("rax"); // optional if void (stack corruption risk if none). Real impl check.
                out << "    leave\n";
                out << "    ret\n";
                break;
            }
            case IROp::Print: {
                popTo("rax");
                out << "    mov rcx, fmt_int\n";      // Windows x64: RCX = format
                out << "    mov rdx, rax\n";          // RDX = value
                out << "    call printf\n";
                break;
            }
            case IROp::Pop:
                popTo("rax");
                break;
            }
        }
        // Provide dummy user function labels if referenced
        for (auto& ins : ir) if (ins.op == IROp::Call) {
            out << mangleUserFunction(ins.s) << ":\n";
            out << "    push rbp\n    mov rbp, rsp\n    sub rsp, 32\n";
            out << "    mov rax, 0\n";
            out << "    leave\n    ret\n";
        }
        return out.str();
    }
private:
    static std::string mangleUserFunction(const std::string& name) {
        return "phoenix_fn_" + name;
    }
};

// Driver
int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: phoenixc <source.phx> [out.asm]\n";
        return 1;
    }
    std::string sourcePath = argv[1];
    std::ifstream in(sourcePath);
    if (!in) {
        std::cerr << "Cannot open source file.\n";
        return 1;
    }
    std::stringstream buffer;
    buffer << in.rdbuf();
    std::string code = buffer.str();

    try {
        Lexer lx(code);
        auto tokens = lx.lex();
        Parser parser(tokens);
        Program prog = parser.parse();
        CodeGen gen;
        gen.generate(prog);

        AsmEmitter emitter;
        std::string asmText = emitter.emit(gen.ir, code);
        std::string outPath = (argc >= 3) ? argv[2] : "out.asm";
        std::ofstream out(outPath);
        out << asmText;
        std::cout << "Assembly written to " << outPath << "\n";
        std::cout << "You can assemble with: nasm -f win64 " << outPath << " -o out.obj && link /subsystem:console out.obj msvcrt.lib legacy_stdio_definitions.lib\n";
    }
    catch (std::exception& ex) {
        std::cerr << "Error: " << ex.what() << "\n";
        return 2;
    }
    return 0;
}

/*
EXTENDING TO FULL LANGUAGE:
1. Insert complete grammar in comments for documentation.
2. Add additional token kinds, expression/statement nodes.
3. Expand semantic analysis (types, control flow validation).
4. Refine calling convention to fully conform to Windows x64 + callee-saved regs.
5. Implement function definitions emission (currently stubbed).
6. Implement constant folding, register allocation optimization.
7. Add actual machine code writer (ELF/PE) if direct machine generation desired.

SIDE-BY-SIDE SOURCE MAPPING:
Enhance IRInstr to carry original line numbers; emit "; line X: original snippet" comments aligned with assembly.

INLINING & OPT:
To add inline NASM inside MSVC for x64 you must use external .asm (MSVC disallows inline asm in x64). This file already emits .asm.

SECURITY:
No untrusted execution; this only generates assembly.

*/

