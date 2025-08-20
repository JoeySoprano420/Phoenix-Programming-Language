// phoenix_transpiler_win.cpp
// Phoenix ProLang → Direct Mapping Transpiler → x86-64 → Instant Execution
// C++17, Windows (MSVC)

#include <windows.h>
#include <cstdint>
#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include <cstring>
using namespace std;

// -------------------------
// Utility: Executable Buffer
// -------------------------
struct CodeBuffer {
    vector<uint8_t> buf;

    void emit(uint8_t b) { buf.push_back(b); }
    void emit32(uint32_t v) {
        for (int i = 0; i < 4; i++) buf.push_back((v >> (i * 8)) & 0xFF);
    }
    void emit64(uint64_t v) {
        for (int i = 0; i < 8; i++) buf.push_back((v >> (i * 8)) & 0xFF);
    }
    uint8_t* data() { return buf.data(); }
    size_t size() { return buf.size(); }
};

// -------------------------
// Executable runner (Windows VirtualAlloc)
// -------------------------
using JitFn = int(*)();

int run(CodeBuffer& cb) {
    LPVOID mem = VirtualAlloc(nullptr, cb.size(),
        MEM_COMMIT | MEM_RESERVE,
        PAGE_EXECUTE_READWRITE);
    if (!mem) {
        cerr << "VirtualAlloc failed\n";
        return -1;
    }
    memcpy(mem, cb.data(), cb.size());
    auto fn = reinterpret_cast<JitFn>(mem);
    int ret = fn();
    VirtualFree(mem, 0, MEM_RELEASE);
    return ret;
}

// -------------------------
// Lexer (tiny demo subset)
// -------------------------
enum class TokKind {
    Eof, Ident, Number, String, LParen, RParen, LBrace, RBrace,
    Fn, Return, Capsule, Log, Semicolon, Comma
};
struct Token { TokKind kind; string text; };

struct Lexer {
    string src; size_t i = 0;
    Lexer(string s) :src(move(s)) {}
    char peek() { return i < src.size() ? src[i] : '\0'; }
    char get() { return i < src.size() ? src[i++] : 0; }
    void skipws() { while (isspace(peek())) get(); }

    Token next() {
        skipws();
        char c = peek();
        if (!c) return { TokKind::Eof,"" };
        if (isalpha(c)) {
            string id;
            while (isalnum(peek())) id += get();
            if (id == "fn") return { TokKind::Fn,id };
            if (id == "return") return { TokKind::Return,id };
            if (id == "capsule") return { TokKind::Capsule,id };
            if (id == "log") return { TokKind::Log,id };
            return { TokKind::Ident,id };
        }
        if (isdigit(c)) {
            string num; while (isdigit(peek())) num += get();
            return { TokKind::Number,num };
        }
        if (c == '"') {
            get(); string s;
            while (peek() != '"' && peek()) s += get();
            get();
            return { TokKind::String,s };
        }
        get();
        switch (c) {
        case '(': return { TokKind::LParen,"(" };
        case ')': return { TokKind::RParen,")" };
        case '{': return { TokKind::LBrace,"{" };
        case '}': return { TokKind::RBrace,"}" };
        case ';': return { TokKind::Semicolon,";" };
        case ',': return { TokKind::Comma,"," };
        }
        return { TokKind::Eof,"" };
    }
};

// -------------------------
// AST (minimal subset)
// -------------------------
struct Expr { virtual ~Expr() = default; };
struct NumExpr :Expr { int val; NumExpr(int v) :val(v) {} };
struct StrExpr :Expr { string val; StrExpr(string v) :val(move(v)) {} };
struct CallExpr :Expr {
    string callee; vector<unique_ptr<Expr>> args;
    CallExpr(string c) :callee(move(c)) {}
};

struct Stmt { virtual ~Stmt() = default; };
struct ReturnStmt :Stmt { unique_ptr<Expr> val; };
struct ExprStmt :Stmt { unique_ptr<Expr> val; };

struct FnDecl {
    string name;
    vector<unique_ptr<Stmt>> body;
};

struct Capsule {
    string name;
    vector<FnDecl> fns;
};

// -------------------------
// Parser (tiny demo subset)
// -------------------------
struct Parser {
    Lexer lex; Token cur;
    Parser(string s) :lex(move(s)) { cur = lex.next(); }
    void adv() { cur = lex.next(); }
    bool eat(TokKind k) { if (cur.kind == k) { adv(); return true; } return false; }

    unique_ptr<Expr> parseExpr() {
        if (cur.kind == TokKind::Number) { int v = stoi(cur.text); adv(); return make_unique<NumExpr>(v); }
        if (cur.kind == TokKind::String) { string s = cur.text; adv(); return make_unique<StrExpr>(s); }
        if (cur.kind == TokKind::Log) {
            adv(); eat(TokKind::LParen);
            auto e = parseExpr();
            eat(TokKind::RParen);
            auto c = make_unique<CallExpr>("log");
            c->args.push_back(move(e));
            return c;
        }
        return nullptr;
    }

    unique_ptr<Stmt> parseStmt() {
        if (cur.kind == TokKind::Return) {
            adv();
            auto e = parseExpr();
            eat(TokKind::Semicolon);
            auto r = make_unique<ReturnStmt>();
            r->val = move(e);
            return r;
        }
        auto e = parseExpr();
        eat(TokKind::Semicolon);
        auto s = make_unique<ExprStmt>();
        s->val = move(e);
        return s;
    }

    FnDecl parseFn() {
        eat(TokKind::Fn);
        string name = cur.text; adv();
        eat(TokKind::LParen); eat(TokKind::RParen);
        eat(TokKind::LBrace);
        FnDecl fn; fn.name = name;
        while (cur.kind != TokKind::RBrace && cur.kind != TokKind::Eof) {
            fn.body.push_back(parseStmt());
        }
        eat(TokKind::RBrace);
        return fn;
    }

    Capsule parseCapsule() {
        eat(TokKind::Capsule);
        string name = cur.text; adv();
        eat(TokKind::LBrace);
        Capsule cap; cap.name = name;
        while (cur.kind != TokKind::RBrace && cur.kind != TokKind::Eof) {
            cap.fns.push_back(parseFn());
        }
        eat(TokKind::RBrace);
        return cap;
    }
};

// -------------------------
// Codegen (x86-64 direct)
// -------------------------
extern "C" int __cdecl puts(const char*);

struct Codegen {
    CodeBuffer cb;

    void prologue() { cb.emit(0x55); cb.emit(0x48); cb.emit(0x89); cb.emit(0xE5); } // push rbp; mov rbp,rsp
    void epilogue() { cb.emit(0x5D); cb.emit(0xC3); } // pop rbp; ret

    void mov_rax_imm64(uint64_t imm) {
        cb.emit(0x48); cb.emit(0xB8);
        cb.emit64(imm);
    }
    void mov_rdi_imm64(uint64_t imm) {
        cb.emit(0x48); cb.emit(0xBF);
        cb.emit64(imm);
    }
    void call_abs(void* fn) {
        cb.emit(0xE8);
        int64_t rel = (int64_t)fn - ((int64_t)cb.size() + 5); // simple relative calc
        cb.emit32((int32_t)rel);
    }

    void genExpr(Expr* e) {
        if (auto n = dynamic_cast<NumExpr*>(e)) {
            mov_rax_imm64(n->val);
        }
        else if (auto s = dynamic_cast<StrExpr*>(e)) {
            mov_rdi_imm64((uint64_t)s->val.c_str());
            call_abs((void*)puts);
        }
        else if (auto c = dynamic_cast<CallExpr*>(e)) {
            if (c->callee == "log") {
                if (auto s2 = dynamic_cast<StrExpr*>(c->args[0].get())) {
                    mov_rdi_imm64((uint64_t)s2->val.c_str());
                    call_abs((void*)puts);
                }
            }
        }
    }

    void genFn(FnDecl& fn) {
        prologue();
        for (auto& st : fn.body) {
            if (auto r = dynamic_cast<ReturnStmt*>(st.get())) {
                genExpr(r->val.get());
            }
            else if (auto ex = dynamic_cast<ExprStmt*>(st.get())) {
                genExpr(ex->val.get());
            }
        }
        epilogue();
    }
};

// -------------------------
// Main
// -------------------------
int main() {
    string src = R"(
        capsule Main {
            fn main() {
                log("Hello Phoenix from MSVC!");
                return 0;
            }
        }
    )";

    Parser p(src);
    Capsule cap = p.parseCapsule();

    Codegen cg;
    cg.genFn(cap.fns[0]); // assume main only

    int ret = run(cg.cb);
    cerr << "[Phoenix exited with " << ret << "]\n";
    return 0;
}

// =============================================
// EXTENSION: Arithmetic + If/While Codegen
// =============================================

// New AST node types
struct BinExpr : Expr {
    string op;
    unique_ptr<Expr> lhs, rhs;
    BinExpr(string o, unique_ptr<Expr> l, unique_ptr<Expr> r)
        : op(move(o)), lhs(move(l)), rhs(move(r)) {
    }
};
struct IfStmt : Stmt {
    unique_ptr<Expr> cond;
    vector<unique_ptr<Stmt>> thenBody;
    vector<unique_ptr<Stmt>> elseBody;
};
struct WhileStmt : Stmt {
    unique_ptr<Expr> cond;
    vector<unique_ptr<Stmt>> body;
};

// Extend Parser with arithmetic and control flow
// (Drop into Parser struct if you want permanent integration)
unique_ptr<Expr> parsePrimary(Parser& P) {
    if (P.cur.kind == TokKind::Number) {
        int v = stoi(P.cur.text); P.adv();
        return make_unique<NumExpr>(v);
    }
    if (P.cur.kind == TokKind::String) {
        string s = P.cur.text; P.adv();
        return make_unique<StrExpr>(s);
    }
    if (P.cur.kind == TokKind::LParen) {
        P.adv();
        auto e = parsePrimary(P);
        P.eat(TokKind::RParen);
        return e;
    }
    return nullptr;
}

// Very simplified expression parser: lhs op rhs
unique_ptr<Expr> parseBinExpr(Parser& P) {
    auto lhs = parsePrimary(P);
    while (P.cur.kind == TokKind::Ident || P.cur.text == "+" || P.cur.text == "-" ||
        P.cur.text == "*" || P.cur.text == "/" || P.cur.text == "==" || P.cur.text == "<") {
        string op = P.cur.text;
        P.adv();
        auto rhs = parsePrimary(P);
        lhs = make_unique<BinExpr>(op, move(lhs), move(rhs));
    }
    return lhs;
}

// Extend Codegen
void genExprArith(Codegen& cg, Expr* e) {
    if (auto b = dynamic_cast<BinExpr*>(e)) {
        // Generate lhs
        genExprArith(cg, b->lhs.get());
        // save rax on stack
        cg.cb.emit(0x50); // push rax
        // Generate rhs
        genExprArith(cg, b->rhs.get());
        // pop into rcx
        cg.cb.emit(0x59); // pop rcx
        // do op: lhs in rcx, rhs in rax
        if (b->op == "+") {
            cg.cb.emit(0x48); cg.cb.emit(0x01); cg.cb.emit(0xC8); // add rax,rcx
        }
        else if (b->op == "-") {
            cg.cb.emit(0x48); cg.cb.emit(0x29); cg.cb.emit(0xC8); // sub rax,rcx
        }
        else if (b->op == "*") {
            cg.cb.emit(0x48); cg.cb.emit(0x0F); cg.cb.emit(0xAF); cg.cb.emit(0xC1); // imul rax,rcx
        }
        else if (b->op == "/") {
            // move rax into rdx:rax for idiv rcx
            cg.cb.emit(0x48); cg.cb.emit(0x99); // cqo
            cg.cb.emit(0x48); cg.cb.emit(0xF7); cg.cb.emit(0xF9); // idiv rcx
        }
        else if (b->op == "==") {
            cg.cb.emit(0x48); cg.cb.emit(0x39); cg.cb.emit(0xC8); // cmp rax,rcx
            cg.cb.emit(0x0F); cg.cb.emit(0x94); cg.cb.emit(0xC0); // sete al
            cg.cb.emit(0x48); cg.cb.emit(0x0F); cg.cb.emit(0xB6); cg.cb.emit(0xC0); // movzx rax,al
        }
        else if (b->op == "<") {
            cg.cb.emit(0x48); cg.cb.emit(0x39); cg.cb.emit(0xC8); // cmp rax,rcx
            cg.cb.emit(0x0F); cg.cb.emit(0x9C); cg.cb.emit(0xC0); // setl al
            cg.cb.emit(0x48); cg.cb.emit(0x0F); cg.cb.emit(0xB6); cg.cb.emit(0xC0); // movzx rax,al
        }
    }
    else if (auto n = dynamic_cast<NumExpr*>(e)) {
        cg.mov_rax_imm64(n->val);
    }
    else if (auto s = dynamic_cast<StrExpr*>(e)) {
        cg.mov_rdi_imm64((uint64_t)s->val.c_str());
        cg.call_abs((void*)puts);
    }
}

// Simple test: arithmetic and if/while
void test_arith_if_while() {
    Codegen cg;
    cg.prologue();

    // Example: return (2+3)*4;
    cg.mov_rax_imm64(2);
    cg.cb.emit(0x50); // push rax
    cg.mov_rax_imm64(3);
    cg.cb.emit(0x59); // pop rcx
    cg.cb.emit(0x48); cg.cb.emit(0x01); cg.cb.emit(0xC8); // add rax,rcx
    cg.cb.emit(0x50); // push rax
    cg.mov_rax_imm64(4);
    cg.cb.emit(0x59); // pop rcx
    cg.cb.emit(0x48); cg.cb.emit(0x0F); cg.cb.emit(0xAF); cg.cb.emit(0xC1); // imul rax,rcx

    cg.epilogue();

    int ret = run(cg.cb);
    cout << "[arith test returned " << ret << "]\n";
}

void test_if_while() {
    Codegen cg;
    cg.prologue();
    // Example: if (x < 10) { return x; } else { return 0; }
    cg.mov_rax_imm64(5); // assume x = 5
    cg.mov_rdi_imm64(10);
    cg.cb.emit(0x48); cg.cb.emit(0x39); cg.cb.emit(0xC8); // cmp rax, rdi
    cg.cb.emit(0x0F); cg.cb.emit(0x8C); // jl label1
    // else part
    cg.mov_rax_imm64(0);
    cg.cb.emit(0xEB); // jmp end
	// if part
    cg.cb.emit(0x90); // label1: nop (placeholder for label)
    cg.mov_rax_imm64(5); // return x
    // end label
    cg.cb.emit(0x90); // end: nop (placeholder for end)
    cg.epilogue();
    int ret = run(cg.cb);
	cout << "[if/while test returned " << ret << "]\n";
}

int main() {
    // Run the main Phoenix capsule
    string src = R"(
        capsule Main {
            fn main() {
                log("Hello Phoenix from MSVC!");
                return 0;
            }
        }
    )";
    Parser p(src);
    Capsule cap = p.parseCapsule();
    Codegen cg;
    cg.genFn(cap.fns[0]); // assume main only
    int ret = run(cg.cb);
    cerr << "[Phoenix exited with " << ret << "]\n";
    // Run arithmetic and if/while tests
    test_arith_if_while();
    test_if_while();
    test_if_else();
    test_while_loop();
    return 0;
}
// Note: The above code is a simplified example and does not include full error handling,
// memory management, or complex features. It is meant to illustrate the basic structure
// and functionality of a Phoenix ProLang to x86-64 JIT compiler using MSVC.

// =============================================
// EXTENSION: If/Else + While Loop Machine Code
// =============================================

// Utility: relative jump helper
void jmp_rel32(CodeBuffer& cb, int32_t rel) {
    cb.emit(0xE9);
    cb.emit32(rel);
}
void jcc_rel32(CodeBuffer& cb, uint8_t cc, int32_t rel) {
    cb.emit(0x0F); cb.emit(cc);
    cb.emit32(rel);
}

// Demo: if/else
// Equivalent Phoenix pseudo-code:
// fn main() {
//    let x = 5;
//    if (x < 10) {
//       return 111;
//    } else {
//       return 222;
//    }
// }
void test_if_else() {
    Codegen cg;
    cg.prologue();

    // mov rax,5
    cg.mov_rax_imm64(5);
    // cmp rax,10
    cg.cb.emit(0x48); cg.cb.emit(0x83); cg.cb.emit(0xF8); cg.cb.emit(10);

    // jge else_label
    size_t jge_pos = cg.cb.size();
    cg.cb.emit(0x0F); cg.cb.emit(0x8D); cg.cb.emit32(0); // jge rel32 placeholder

    // THEN: return 111
    cg.mov_rax_imm64(111);
    size_t jmp_end_pos = cg.cb.size();
    jmp_rel32(cg.cb, 0); // jump to end placeholder

    // ELSE label target
    size_t else_label = cg.cb.size();
    cg.mov_rax_imm64(222);

    // Patch jge to point here
    int32_t rel_else = (int32_t)(else_label - (jge_pos + 6));
    memcpy(&cg.cb.buf[jge_pos + 2], &rel_else, 4);

    // END label target
    size_t end_label = cg.cb.size();

    // Patch unconditional jump to end
    int32_t rel_end = (int32_t)(end_label - (jmp_end_pos + 5));
    memcpy(&cg.cb.buf[jmp_end_pos + 1], &rel_end, 4);

    cg.epilogue();

    int ret = run(cg.cb);
    cout << "[if/else test returned " << ret << "]\n";
}

// Demo: while loop
// Equivalent Phoenix pseudo-code:
// fn main() {
//    let i = 0;
//    while (i < 5) {
//       i = i + 1;
//    }
//    return i;
// }
void test_while_loop() {
    Codegen cg;
    cg.prologue();

    // mov rax,0   ; i=0
    cg.mov_rax_imm64(0);

    size_t loop_start = cg.cb.size();

    // cmp rax,5
    cg.cb.emit(0x48); cg.cb.emit(0x83); cg.cb.emit(0xF8); cg.cb.emit(5);

    // jge end_label
    size_t jge_pos = cg.cb.size();
    cg.cb.emit(0x0F); cg.cb.emit(0x8D); cg.cb.emit32(0); // placeholder

    // i = i + 1
    cg.cb.emit(0x48); cg.cb.emit(0x83); cg.cb.emit(0xC0); cg.cb.emit(1);

    // jmp loop_start
    int32_t rel_back = (int32_t)(loop_start - (cg.cb.size() + 5));
    jmp_rel32(cg.cb, rel_back);

    // END label
    size_t end_label = cg.cb.size();
    int32_t rel_end = (int32_t)(end_label - (jge_pos + 6));
    memcpy(&cg.cb.buf[jge_pos + 2], &rel_end, 4);

    // return i in rax
    cg.epilogue();

    int ret = run(cg.cb);
    cout << "[while test returned " << ret << "]\n";
}

// Note: The above code is a simplified example and does not include full error handling,

// =============================================
// EXTENSION: Parser + Codegen for if/else + while
// =============================================

// -------------------------
// AST for control flow
// -------------------------
struct IfStmt : Stmt {
    unique_ptr<Expr> cond;
    vector<unique_ptr<Stmt>> thenBody;
    vector<unique_ptr<Stmt>> elseBody;
};
struct WhileStmt : Stmt {
    unique_ptr<Expr> cond;
    vector<unique_ptr<Stmt>> body;
};
struct AssignStmt : Stmt {
    string name;
    unique_ptr<Expr> value;
};

// -------------------------
// Parser: add if/else + while
// -------------------------
unique_ptr<Stmt> parseIf(Parser& P) {
    P.eat(TokKind::Ident); // "if"
    P.eat(TokKind::LParen);
    auto cond = P.parseExpr();
    P.eat(TokKind::RParen);

    P.eat(TokKind::LBrace);
    vector<unique_ptr<Stmt>> thenStmts;
    while (P.cur.kind != TokKind::RBrace && P.cur.kind != TokKind::Eof) {
        thenStmts.push_back(P.parseStmt());
    }
    P.eat(TokKind::RBrace);

    vector<unique_ptr<Stmt>> elseStmts;
    if (P.cur.text == "else") {
        P.adv();
        P.eat(TokKind::LBrace);
        while (P.cur.kind != TokKind::RBrace && P.cur.kind != TokKind::Eof) {
            elseStmts.push_back(P.parseStmt());
        }
        P.eat(TokKind::RBrace);
    }

    auto ifs = make_unique<IfStmt>();
    ifs->cond = move(cond);
    ifs->thenBody = move(thenStmts);
    ifs->elseBody = move(elseStmts);
    return ifs;
}

unique_ptr<Stmt> parseWhile(Parser& P) {
    P.eat(TokKind::Ident); // "while"
    P.eat(TokKind::LParen);
    auto cond = P.parseExpr();
    P.eat(TokKind::RParen);

    P.eat(TokKind::LBrace);
    vector<unique_ptr<Stmt>> body;
    while (P.cur.kind != TokKind::RBrace && P.cur.kind != TokKind::Eof) {
        body.push_back(P.parseStmt());
    }
    P.eat(TokKind::RBrace);

    auto w = make_unique<WhileStmt>();
    w->cond = move(cond);
    w->body = move(body);
    return w;
}

// Extend Parser::parseStmt to recognize if/while
// (you can inline this back into Parser for permanence)
unique_ptr<Stmt> extendedParseStmt(Parser& P) {
    if (P.cur.text == "if") return parseIf(P);
    if (P.cur.text == "while") return parseWhile(P);
    return P.parseStmt(); // fallback to old statements
}

// -------------------------
// Codegen: add if/else + while
// -------------------------
void genStmt(Codegen& cg, Stmt* st);

void genExprFull(Codegen& cg, Expr* e) {
    if (auto n = dynamic_cast<NumExpr*>(e)) {
        cg.mov_rax_imm64(n->val);
    }
    else if (auto s = dynamic_cast<StrExpr*>(e)) {
        cg.mov_rdi_imm64((uint64_t)s->val.c_str());
        cg.call_abs((void*)puts);
    }
    else {
        // Fallback to arithmetic handler
        genExprArith(cg, e);
    }
}

void genIf(Codegen& cg, IfStmt* ifs) {
    // Evaluate condition → result in rax
    genExprFull(cg, ifs->cond.get());

    // cmp rax,0
    cg.cb.emit(0x48); cg.cb.emit(0x83); cg.cb.emit(0xF8); cg.cb.emit(0);

    // je else
    size_t je_pos = cg.cb.size();
    cg.cb.emit(0x0F); cg.cb.emit(0x84); cg.cb.emit32(0);

    // thenBody
    for (auto& s : ifs->thenBody) genStmt(cg, s.get());

    // jmp end
    size_t jmp_pos = cg.cb.size();
    cg.cb.emit(0xE9); cg.cb.emit32(0);

    // else label
    size_t else_label = cg.cb.size();
    for (auto& s : ifs->elseBody) genStmt(cg, s.get());

    // Patch je
    int32_t rel_else = (int32_t)(else_label - (je_pos + 6));
    memcpy(&cg.cb.buf[je_pos + 2], &rel_else, 4);

    // end label
    size_t end_label = cg.cb.size();
    int32_t rel_end = (int32_t)(end_label - (jmp_pos + 5));
    memcpy(&cg.cb.buf[jmp_pos + 1], &rel_end, 4);
}

void genWhile(Codegen& cg, WhileStmt* ws) {
    size_t loop_start = cg.cb.size();

    genExprFull(cg, ws->cond.get());
    // cmp rax,0
    cg.cb.emit(0x48); cg.cb.emit(0x83); cg.cb.emit(0xF8); cg.cb.emit(0);

    // je end
    size_t je_pos = cg.cb.size();
    cg.cb.emit(0x0F); cg.cb.emit(0x84); cg.cb.emit32(0);

    // body
    for (auto& s : ws->body) genStmt(cg, s.get());

    // jmp loop_start
    int32_t rel_back = (int32_t)(loop_start - (cg.cb.size() + 5));
    cg.cb.emit(0xE9); cg.cb.emit32(rel_back);

    // end
    size_t end_label = cg.cb.size();
    int32_t rel_end = (int32_t)(end_label - (je_pos + 6));
    memcpy(&cg.cb.buf[je_pos + 2], &rel_end, 4);
}

void genStmt(Codegen& cg, Stmt* st) {
    if (auto r = dynamic_cast<ReturnStmt*>(st)) {
        genExprFull(cg, r->val.get());
    }
    else if (auto ex = dynamic_cast<ExprStmt*>(st)) {
        genExprFull(cg, ex->val.get());
    }
    else if (auto iff = dynamic_cast<IfStmt*>(st)) {
        genIf(cg, iff);
    }
    else if (auto wh = dynamic_cast<WhileStmt*>(st)) {
        genWhile(cg, wh);
    }
}

// -------------------------
// TEST MAIN with if/while
// -------------------------
void test_phx_if_while() {
    string src = R"(
        capsule Main {
            fn main() {
                let mut i = 0;
                while (i < 5) {
                    i = i + 1;
                }
                if (i == 5) {
                    return 123;
                } else {
                    return 999;
                }
            }
        }
    )";

    Parser p(src);
    Capsule cap = p.parseCapsule();

    Codegen cg;
    cg.genFn(cap.fns[0]); // main
    int ret = run(cg.cb);

    cout << "[phx if/while returned " << ret << "]\n";
}

// =============================================
// EXTENSION: Variable storage + assignments
// =============================================

// Symbol table entry: local variable with stack offset
struct VarInfo {
    int offset;   // negative offset from RBP
    bool isMut;
};

// Simple environment for locals
struct Env {
    unordered_map<string, VarInfo> locals;
    int stackOffset = -8; // start below rbp
};

#include <unordered_map>
// Extend AST
struct LetStmt : Stmt {
    string name;
    bool isMut;
    unique_ptr<Expr> init;
};
struct AssignStmt : Stmt {
    string name;
    unique_ptr<Expr> value;
};
struct VarExpr : Expr {
    string name;
    VarExpr(string n) :name(move(n)) {}
};

// -------------------------
// Parser: recognize let and assignment
// -------------------------
unique_ptr<Stmt> parseLet(Parser& P) {
    P.eat(TokKind::Ident); // 'let'
    bool isMut = false;
    if (P.cur.text == "mut") { P.adv(); isMut = true; }

    string name = P.cur.text; P.adv();
    P.eat(TokKind::Ident); // consume name
    P.eat(TokKind::Ident); // '='
    auto init = P.parseExpr();
    P.eat(TokKind::Semicolon);

    auto l = make_unique<LetStmt>();
    l->name = name; l->isMut = isMut; l->init = move(init);
    return l;
}

unique_ptr<Stmt> parseAssign(Parser& P, string name) {
    // already saw name
    P.eat(TokKind::Ident); // '='
    auto val = P.parseExpr();
    P.eat(TokKind::Semicolon);
    auto a = make_unique<AssignStmt>();
    a->name = name; a->value = move(val);
    return a;
}

// Extend Parser::parseExpr to handle variables
unique_ptr<Expr> parseVarOrExpr(Parser& P) {
    if (P.cur.kind == TokKind::Ident) {
        string name = P.cur.text;
        P.adv();
        return make_unique<VarExpr>(name);
    }
    return P.parseExpr();
}

// -------------------------
// Codegen with locals
// -------------------------
struct CodegenWithEnv : Codegen {
    Env env;

    void genExprVar(Expr* e) {
        if (auto v = dynamic_cast<VarExpr*>(e)) {
            auto it = env.locals.find(v->name);
            if (it == env.locals.end()) {
                cerr << "Unknown var: " << v->name << "\n"; return;
            }
            int off = it->second.offset;
            cb.emit(0x48); cb.emit(0x8B); cb.emit(0x45); cb.emit((uint8_t)off); // mov rax,[rbp+off]
        }
        else {
            genExprFull(*this, e);
        }
    }

    void genStmtWithEnv(Stmt* st) {
        if (auto l = dynamic_cast<LetStmt*>(st)) {
            env.locals[l->name] = { env.stackOffset,l->isMut };
            genExprVar(l->init.get());
            // mov [rbp+offset],rax
            cb.emit(0x48); cb.emit(0x89); cb.emit(0x45); cb.emit((uint8_t)env.stackOffset);
            env.stackOffset -= 8;
        }
        else if (auto a = dynamic_cast<AssignStmt*>(st)) {
            auto it = env.locals.find(a->name);
            if (it == env.locals.end()) { cerr << "Unknown assign var\n"; return; }
            genExprVar(a->value.get());
            cb.emit(0x48); cb.emit(0x89); cb.emit(0x45); cb.emit((uint8_t)it->second.offset);
        }
        else if (auto r = dynamic_cast<ReturnStmt*>(st)) {
            genExprVar(r->val.get());
        }
        else if (auto iff = dynamic_cast<IfStmt*>(st)) {
            genIf(*this, iff);
        }
        else if (auto wh = dynamic_cast<WhileStmt*>(st)) {
            genWhile(*this, wh);
        }
        else if (auto ex = dynamic_cast<ExprStmt*>(st)) {
            genExprVar(ex->val.get());
        }
    }
};

// -------------------------
// Test Phoenix with locals
// -------------------------
void test_phx_locals() {
    string src = R"(
        capsule Main {
            fn main() {
                let mut i = 0;
                while (i < 5) {
                    i = i + 1;
                }
                return i;
            }
        }
    )";

    Parser p(src);
    Capsule cap = p.parseCapsule();

    CodegenWithEnv cg;
    cg.prologue();

    for (auto& st : cap.fns[0].body) {
        cg.genStmtWithEnv(st.get());
    }

    cg.epilogue();
    int ret = run(cg.cb);

    cout << "[phx locals test returned " << ret << "]\n";
}

int main() {
    // Run the main Phoenix capsule
    test_phx_if_while();
    // Run arithmetic and if/while tests
    test_arith_if_while();
    test_if_else();
    test_while_loop();
    test_phx_locals();
    return 0;
}

