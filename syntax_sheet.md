## This is what compiler devs, users, and tools (like GPTs) need to understand the **full shape of the language**.

---

# 🔥 **Phoenix ProLang — Syntax Specification (v1.0 Draft)**

---

## 📦 Capsules (Modules)

```ebnf
program        ::= capsule_decl+

capsule_decl   ::= "capsule" IDENT "{" capsule_item* "}"
capsule_item   ::= global_decl | fn_decl | struct_decl | enum_decl | import_decl | export_decl
```

---

## 🌍 Global Declarations

```ebnf
global_decl    ::= "let" ["mut"] IDENT [ ":" type ] "=" expr ";"
export_decl    ::= "export" global_decl | "export" fn_decl
import_decl    ::= "import" IDENT ["as" IDENT] ";"
```

✅ Globals default to immutable.
✅ Only `export let mut` can be reassigned across capsules.
✅ Imports may be aliased:

```phoenix
import Counter as C;
C.counter = 42;
```

---

## 🧩 Types

```ebnf
type           ::= "int" | "float" | "bool" | "char" | "string" | "null"
                 | IDENT
                 | "[" type ";" constexpr_expr "]"      // fixed-size array
                 | "list" "<" type ">"                  // dynamic list
                 | "struct" | "enum" | "union"          // user types
```

---

## 🔧 Structs, Enums, Unions

```ebnf
struct_decl    ::= "struct" IDENT "{" field_decl* "}"
field_decl     ::= "let" ["mut"] IDENT ":" type ";"

enum_decl      ::= "enum" IDENT "{" enum_variant ("," enum_variant)* "}"
enum_variant   ::= IDENT [ "(" type ")" ]

union_decl     ::= "union" IDENT "{" field_decl* "}"
```

Immutability applies to fields:

```phoenix
struct Foo {
    let x: int;       // immutable
    let mut y: int;   // mutable
}
```

---

## 🏗 Functions

```ebnf
fn_decl        ::= fn_header block
fn_header      ::= ["constexpr"] ["noexcept"] "fn" IDENT "(" param_list? ")" [ ":" type ]
param_list     ::= param ("," param)*
param          ::= ["mut"] IDENT ":" type
```

Examples:

```phoenix
fn add(x: int, y: int): int { return x + y; }
constexpr fn factorial(n: int): int { if (n <= 1) return 1; return n * factorial(n - 1); }
noexcept fn fast_add(a: int, b: int): int { return a + b; }
```

---

## 🧠 Statements

```ebnf
block          ::= "{" stmt* "}"
stmt           ::= let_stmt
                 | assign_stmt
                 | expr_stmt
                 | if_stmt
                 | while_stmt
                 | loop_stmt
                 | break_stmt
                 | continue_stmt
                 | return_stmt
                 | try_stmt
                 | throw_stmt

let_stmt       ::= "let" ["mut"] IDENT [ ":" type ] "=" expr ";"
assign_stmt    ::= (IDENT | field_access | index_expr) "=" expr ";"
expr_stmt      ::= expr ";"
if_stmt        ::= "if" "(" expr ")" block ["else" block]
while_stmt     ::= "while" "(" expr ")" block
loop_stmt      ::= "loop" block
break_stmt     ::= "break" ";"
continue_stmt  ::= "continue" ";"
return_stmt    ::= "return" expr? ";"
try_stmt       ::= "try" block "catch" "(" IDENT ":" type ")" block
throw_stmt     ::= "throw" expr ";"
```

---

## 🧮 Expressions

```ebnf
expr           ::= logic_or
logic_or       ::= logic_and ("||" logic_and)*
logic_and      ::= equality ("&&" equality)*
equality       ::= comparison (("==" | "!=") comparison)*
comparison     ::= term (("<" | ">" | "<=" | ">=") term)*
term           ::= factor (("+" | "-") factor)*
factor         ::= unary (("*" | "/" | "%") unary)*
unary          ::= ("!" | "-" | "++" | "--") unary
                 | primary
primary        ::= literal
                 | IDENT
                 | call_expr
                 | field_access
                 | index_expr
                 | "(" expr ")"

call_expr      ::= IDENT "(" [ expr ("," expr)* ] ")"
field_access   ::= expr "." IDENT
index_expr     ::= expr "[" expr "]"
```

---

## 🔡 Literals

```ebnf
literal        ::= NUMBER | FLOAT | STRING | CHAR | "true" | "false" | "null"
```

Examples:

```phoenix
42
3.14
"hello"
'a'
true
null
```

---

## 🔀 Control Flow Keywords

* `if`, `else` — conditional branching
* `while`, `loop` — loops
* `break`, `continue`
* `return`
* `try`, `catch`, `throw`
* `yield` (for coroutines)

---

## 🔒 Concurrency Primitives

```phoenix
let m = mutex();
lock(m);
unlock(m);

let t = thread(worker, 1000);
join(t);
```

* `mutex`, `lock`, `unlock`
* `thread`, `join`, `detach`
* `atomic`, `volatile` (future)

---

## 🧰 Memory Management

```phoenix
let p = new Foo();   // allocate
delete p;            // free

let mut x = ref y;   // reference
move y;              // move ownership
copy z;              // copy value
```

---

## 📊 Traits & Generics

```ebnf
trait_decl     ::= "trait" IDENT "{" fn_signature* "}"
impl_decl      ::= "impl" IDENT "for" type "{" fn_decl* "}"

generic_fn     ::= "fn" IDENT "<" generic_param_list ">" "(" param_list ")" block
generic_param_list ::= generic_param ("," generic_param)*
generic_param  ::= IDENT [ ":" trait_bound ]
trait_bound    ::= IDENT
```

Example:

```phoenix
trait Addable {
    fn add(self, other: Self): Self;
}

impl Addable for int {
    fn add(self, other: int): int { return self + other; }
}

fn sum<T: Addable>(a: T, b: T): T {
    return a.add(b);
}
```

---

## 🧬 Compile-Time (Constexpr)

* `constexpr fn` evaluated at compile-time
* usable in **types** and **array sizes**

```phoenix
constexpr fn factorial(n: int): int {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

let arr: [int; factorial(5)];
```

---

## 🎛 Panic / Exception Model

* `throw expr;`
* `try { ... } catch(e: Error) { ... }`
* Functions may be marked `noexcept` (cannot throw).
* Global `panic = unwind | abort`.

  * `abort` emits `ud2` or `exit(1)`.
  * `unwind` runs destructors.

---

## 🎹 RAII & Destructors

```phoenix
struct File {
    let mut fd: int;

    fn drop(self) {
        close(self.fd);
    }
}
```

Destructors (`drop`) always run at scope exit (including panic unwinds).

---

# ✅ Summary

Phoenix syntax is:

* **Capsule-driven** modular structure (`capsule`, `import`, `export`)
* **Immutable by default** (`let` vs `let mut`)
* **Rich control flow** (`if`, `while`, `loop`, `try/catch/throw`, `yield`)
* **Memory model** (`new`, `delete`, `ref`, `move`, `copy`)
* **Concurrency** (`thread`, `mutex`, `lock/unlock`, `atomic`)
* **Traits & Generics** with compile-time evaluation (`constexpr`, trait bounds)
* **RAII & panic modes** for zero-cost safety

---

⚡ This is now a **full syntax sheet** suitable for writing a Phoenix grammar, building IDE tooling, or expanding the compiler pipeline.

---

