# Phoenix-Programming-Language

## Phoenix .phx → Parser → AST → SSA → RegAlloc → x86-64 Asm (.s) → nasm/ld → Executable (.exe)

## 🔥 This is both a **language reference** and a **manifesto**, pulling together all the features we’ve built step by step: VM, parser, SSA, LLVM, ASM, RAII, noexcept, generics, traits, panic modes, optimizations, etc.

---

# 🔥 Phoenix ProLang — **Massive Monolithic Overview (MMO)**

---

## 1. Philosophy

Phoenix ProLang is a **capsule-driven, machine-native programming language**.

It combines **C++’s raw power**, **Rust’s zero-cost safety**, and **LLVM’s backend optimizations** in a design that is:

* **Clarity-first** → minimal, predictable syntax.
* **Safety-first** → immutability by default, destructors, noexcept guarantees, panic modes.
* **Performance-first** → compiles through SSA → LLVM IR → x86-64 assembly, with inlining, loop unrolling, constant folding, and interprocedural optimizations.
* **Traceable** → structured logging, profiling, introspection.
* **Modular** → capsules, imports/exports, generics, traits.

---

## 2. Core Primitives

Phoenix supports a **compact set of fundamental building blocks**:

* **Types**:

  * `int` (64-bit signed)
  * `float` (64-bit IEEE-754)
  * `bool` (0 or 1)
  * `char` (UTF-8 codepoint)
  * `string` (immutable UTF-8)
  * `null` (unit-like sentinel)
  * `struct`, `enum`, `union` (user-defined types)
  * arrays (`[T; N]`) and vectors (`[T]`)

* **Control Structures**:

  * `if`, `else`
  * `while`, `loop`, `for`
  * `break`, `continue`
  * `return`, `yield`, `exit`
  * `try`, `catch`, `throw`

* **Memory & Resource Management**:

  * `new`, `delete`
  * `ref`, `move`, `copy`
  * **RAII destructors**: `fn drop(self)` called automatically at scope exit or unwind.

* **Concurrency**:

  * `thread(fn, arg)`
  * `join(thread)`
  * `mutex`, `lock`, `unlock`
  * `atomic`, `volatile`, `sync`
  * panic-safe unwinding across threads

* **Capsules (Modules)**:

  * `capsule Name { ... }`
  * `export` for functions/globals/types
  * `import OtherCapsule` or `import OtherCapsule as Alias`
  * symbol resolution with alias scoping (`Alias.fn()`, `Alias.var`)

---

## 3. Mutability Model

* **Immutable by default** → `let x = 1;` cannot be reassigned.
* **Explicit mutability** → `let mut x = 1;` allows reassignment.
* **Consistent enforcement**:

  * Locals, globals, struct fields, array/vector elements.
  * Function parameters immutable unless declared `mut`.
  * Imports preserve mutability — cannot reassign an imported `let`, but can reassign `let mut`.
* **Static check** in parser ensures immutability violations are compile errors, not runtime crashes.

---

## 4. Error Handling & Panic Modes

* **Structured exceptions**:

  ```phoenix
  try {
      risky();
  } catch (e: string) {
      log("Error:", e);
  }
  ```
* **Panic-safe unwinding**: destructors (`drop`) run during throws.
* **noexcept functions**:

  * Declared with `fn foo() noexcept`
  * Guaranteed not to throw → lowered to `nounwind` in LLVM.
* **panic = abort mode**:

  * Compiler flag → no unwinding, destructors skipped.
  * Lowered to `ud2` or `exit(1)` in x86-64.

---

## 5. Generics & Compile-Time Features

* **Constexpr functions**: evaluated at compile-time.

  ```phoenix
  constexpr fn factorial(n: int): int { ... }
  let arr: [int; factorial(5)];
  ```

* **Generics**:

  ```phoenix
  fn max<T: Comparable>(a: T, b: T): T { ... }
  ```

* **Monomorphization**: each generic instantiation compiled into specialized code.

* **Deduplication**: identical instantiations merged.

* **Trait bounds (typeclasses)**:

  ```phoenix
  trait Addable { fn add(self, other: Self): Self; }
  impl Addable for int { fn add(self, other: int): int { return self + other; } }
  ```

* **Trait objects (vtables)** for dynamic dispatch.

---

## 6. Execution Model

### 6.1 Interpreter VM

* Stack-based execution.
* AST → executed directly.
* Thread-safe with per-thread state.
* Intrinsics implemented in Python (log, mutex, thread, join).

### 6.2 SSA → LLVM Backend

* AST lowered into Phoenix IR → Static Single Assignment (SSA).
* **Optimizations**:

  * Constant folding
  * Peephole optimizations
  * Loop unrolling (when bounds known)
  * Tail-call optimization (TCO)
  * Dead code elimination (DCE)
  * Common subexpression elimination (CSE)
  * Interprocedural constant folding (pure+noexcept calls)
  * Function inlining (esp. noexcept)
* Lowered to LLVM IR, then to x86-64.

### 6.3 ASM Backend

* Final target: `.s` files assembled by NASM + linked by `ld`.
* Supports:

  * Symbol export/import → maps to `global`/`extern` in NASM.
  * RAII destructors lowered to scope-exit calls.
  * `noexcept` lowers to `nounwind`.
  * Panic = abort → emitted as `ud2`/`exit(1)`.

---

## 7. Standard Library

Currently includes:

* Logging: `log`
* Concurrency: `thread`, `join`, `mutex`, `lock`, `unlock`
* Error handling: `throw`, `try`, `catch`
* Math (planned)
* Strings, I/O, Random, Time (planned)

---

## 8. Example: Phoenix Rosetta Stone

```phoenix
capsule Main {
    import Util as U;
    import Counter;
    import Data;

    fn main(): int {
        log("Demo start");
        let mut s = U.factorial(5);
        log("factorial(5) =", s);

        let t1 = thread(Counter.worker, 100000);
        let t2 = thread(Counter.worker, 100000);
        join(t1); join(t2);
        log("counter =", Counter.counter);

        try {
            let res = Data.risky_div(10, 0);
        } catch(e: string) {
            log("Caught:", e);
        }

        return 0;
    }
}
```

---

## 9. Tagline

⚡ **Phoenix ProLang** — capsule-driven, machine-native, zero-cost safety with blazing x64 speed.

---

✅ At this stage Phoenix is:

* A **real language** (syntax, parser, VM, backend).
* With **safety guarantees** (immutability, RAII, noexcept).
* With **modern optimizations** (SSA, inlining, loop unrolling).
* With a path to **native executables** via ASM.

---

