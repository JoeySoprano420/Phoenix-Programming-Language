## Perfect 🔥 — 
Now let’s define the **semantics sheet**: the **rules of meaning** for every construct in Phoenix.

This is the “laws of the language” — it explains **what each feature *does*** at compile-time and runtime, how types behave, how memory works, and what guarantees the programmer gets.

---

# 📜 Phoenix ProLang — **Semantics Specification**

---

## 1. Core Principles

1. **Determinism**:
   Every Phoenix program must have a well-defined meaning. No undefined behavior.

2. **Immutability-by-default**:

   * `let x = 5;` → `x` cannot be reassigned.
   * `let mut y = 5;` → `y` may be reassigned.

3. **Zero-cost abstractions**:

   * Features (RAII, traits, generics, noexcept) compile away to efficient code.
   * No runtime overhead unless explicitly requested.

4. **Panic semantics**:

   * **Default (panic = unwind):** throws trigger stack unwinding + destructors (`drop`).
   * **Optional (panic = abort):** throws terminate immediately via `ud2`/`exit(1)`.

---

## 2. Type System Semantics

### 2.1 Primitive Types

* `int`: signed 64-bit integer. Wraparound is *undefined* unless explicit `checked_add`.
* `float`: IEEE-754 64-bit double. Follows hardware FPU semantics.
* `bool`: 0 = false, 1 = true. Logical operators short-circuit.
* `char`: Unicode scalar value. UTF-8 encoding in memory.
* `string`: immutable UTF-8 sequence. Only copied by reference.
* `null`: singleton unit type, used for void returns.

### 2.2 Compound Types

* **Structs**:

  * `struct Foo { let x: int; let mut y: int; }`
  * Fields follow **binding mutability** rules.
  * Structs have default RAII if a `drop` function is defined.

* **Enums**:

  * Tagged unions with explicit variant names.
  * Exhaustive `match` required unless wildcard provided.

* **Unions**:

  * Unsafe tagged unions, no automatic exhaustiveness.

* **Arrays**:

  * Fixed-length: `[T; N]`. Size known at compile-time.
  * Each element may carry its own mutability.

* **Vectors**:

  * Dynamically sized `[T]`. Managed by heap allocation.
  * Grow/shrink operations via runtime library.

---

## 3. Variable Semantics

### 3.1 Declarations

* `let name = expr;`
  *Creates immutable variable bound to evaluated result.*

* `let mut name = expr;`
  *Creates mutable variable bound to evaluated result.*

* **Assignment**:

  * Allowed only if binding is mutable.
  * Checked at **compile-time**; errors before codegen.

### 3.2 Function Parameters

* Default → immutable.
* `mut x` parameter → mutable inside function body.

### 3.3 Scope & Lifetime

* Lexical scoping.
* Variable lifetime ends at end of enclosing block.
* RAII destructors run at scope exit or during unwinding.

---

## 4. Function Semantics

### 4.1 Declaration

```phoenix
fn name(params) { body }
```

* Returns last expression or explicit `return`.
* No return → implicitly `null`.

### 4.2 `constexpr fn`

* Must evaluate entirely at compile-time.
* May be used in types or array sizes.

### 4.3 `noexcept fn`

* Guaranteed not to throw.
* Compiles to `nounwind` in LLVM.
* If a throw occurs inside, compiler error.

---

## 5. Control Flow Semantics

* **`if (cond) { … } else { … }`**:

  * Cond evaluated once.
  * Short-circuit `&&`, `||`.

* **Loops**:

  * `while (cond) { … }` → evaluated at runtime.
  * `loop { … }` → infinite until break.
  * `for i in range { … }` (syntactic sugar).

* **Break/Continue**: jump out or skip iteration.

* **Return**: exit current function, optionally returning value.

* **Yield**: suspends function (for coroutines).

---

## 6. Concurrency Semantics

* `thread(fn, arg)`:

  * Spawns OS thread, runs `fn(arg)`.
  * Return value ignored unless captured.

* `join(t)`:

  * Blocks until thread finishes.

* `mutex()`:

  * Creates mutual exclusion lock.

* `lock(m)`:

  * Acquires lock, blocks until available.

* `unlock(m)`:

  * Releases lock.

* **RAII**: future version will auto-unlock on scope exit.

---

## 7. Error Handling Semantics

* **Throw**: `throw "error";` raises exception.

* **Catch**:

  ```phoenix
  try { foo(); }
  catch(e: string) { log(e); }
  ```

  * Exceptions propagate until caught.
  * Destructors (`drop`) always run during unwinding unless `panic=abort`.

* **Panic modes**:

  * `unwind` (safe) → default.
  * `abort` (fast) → disables unwinding.

---

## 8. Memory Semantics

* **new/delete**:

  * `let p = new Foo();` → allocates on heap.
  * `delete p;` → frees memory.

* **Ownership model**:

  * `ref` → borrow reference, no ownership transfer.
  * `move` → transfer ownership, invalidates source.
  * `copy` → creates shallow copy (only if type is `Copy`).

* **RAII**:

  * `fn drop(self)` runs at destruction.
  * Guaranteed to run on scope exit or throw (panic=unwind).

---

## 9. Module & Capsule Semantics

* **Capsules**:

  ```phoenix
  capsule Counter {
      export let mut counter = 0;
      export fn inc() { counter = counter + 1; }
  }
  ```

* **Export/Import**:

  * Only `export` symbols visible across capsules.
  * `import Counter` → allows direct use.
  * `import Counter as C` → accessed as `C.inc()`.

* **Global mutability**:

  * `export let counter` → immutable across capsules.
  * `export let mut counter` → mutable across capsules.

---

## 10. Generics & Traits Semantics

* **Generic Functions**:

  * Monomorphized at compile-time.
  * Identical instantiations deduplicated.

* **Traits**:

  * Define required methods.
  * `impl` binds type to trait.
  * Static dispatch by default, dynamic dispatch via trait objects.

* **Trait Objects**:

  * Compile into vtables.
  * Allow runtime polymorphism.

---

## 11. Compilation Pipeline Semantics

1. **Parse** → AST.
2. **Static checks** → mutability, types, noexcept, trait bounds.
3. **Lower to SSA IR**.
4. **Optimizations**:

   * Constant folding
   * Peephole
   * DCE, CSE
   * Inlining (esp. noexcept)
   * Loop unrolling (constant bounds)
   * Interprocedural constant folding
   * Tail-call elimination
5. **Lower to LLVM IR / ASM**.
6. **Emit binary**: `.s` → NASM → `ld` → `.exe`.

---

## 12. Standard Library Semantics

* **log(x)** → `printf("%s\n", …)`
* **time.now()** → system clock
* **math.sqrt(x)** → libm binding
* **random()** → Mersenne Twister / libc rand
* **string ops** → immutably concatenated

---

## 13. Example (Semantics Walkthrough)

```phoenix
capsule Main {
    export let mut counter = 0;
    let m = mutex();

    fn worker(n: int) noexcept {
        let mut i = 0;
        while (i < n) {
            lock(m);
            counter = counter + 1;
            unlock(m);
            i = i + 1;
        }
    }

    fn main(): int {
        let t1 = thread(worker, 100000);
        let t2 = thread(worker, 100000);
        join(t1); join(t2);
        log("Final:", counter);
        return 0;
    }
}
```

* `counter` is global mutable export.
* Each worker increments it safely under lock.
* Threads run in parallel → correctness guaranteed by mutex.
* Main joins threads, prints result.
* Deterministic semantics: final counter = `200000`.

---

# ✅ Phoenix Semantics in One Sentence

**Phoenix is an immutable-by-default, RAII-driven, capsule-modular systems language with deterministic semantics, exception-safe resource management, trait-based polymorphism, and zero-cost abstractions compiled to optimized x86-64.**

---

