// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -verify %s

// Use-after-consume / use-before-init diagnostics emitted by LifetimeResolution
// and LifetimeResolutionDiagnose.

class C {}
func use(_ c: C) {}
func take(_ c: __owned C) {}

// A loadable noncopyable type: it just wraps a class reference.
struct NC: ~Copyable { let c: C }
func useNC(_ x: borrowing NC) {}
func takeNC(_ x: consuming NC) {}

func useBeforeInitOnOnePath(_ b: Bool) {
  let x: C // expected-note {{constant defined here}}
  if b {
    x = C()
    take(x)
  }
  use(x) // expected-error {{constant 'x' used before being initialized}}
}

func lastUseOnOneBranchConsumes(_ b: Bool) {
  let x: C
  x = C()
  if b {
    take(x)
    return
  }
  use(x)
}

func borrowThenConsume() {
  let x: C
  x = C()
  use(x)
  take(x)
}

func borrowThenConsumeInline() {
  let x = C()
  use(x)
  take(x)
}

func consumeThenUse() {
  let x: C
  x = C()
  take(x)   // interior consume: legal, C is copyable
  use(x)    // still live via the copy above
}

func consumeOnBothBranches(_ b: Bool) {
  let x: C
  x = C()
  if b {
    take(x)
  } else {
    take(x)
  }
}

func initOnBothBranches(_ b: Bool) {
  let x: C
  if b {
    x = C()
  } else {
    x = C()
  }
  use(x)    // valid: initialized on every path
  take(x)
}

func consumeInLoop(_ b: Bool) {
  let x: C
  x = C()
  while b {
    take(x)   // consumed once per iteration; legal, C is copyable
  }
}

func consumeOperatorInLoop(_ b: Bool) {
  let x: C
  x = C()
  while b {
    _ = consume x  // expected-error {{'x' used after consume}} // expected-note {{consumed here}}
  }
}

// A copyable `var` is backed by an alloc_box, unlike a `let`'s alloc_stack.

func varReassignAfterConsume() {
  var x: C
  x = C()
  take(x)   // consume
  x = C()   // reassign revives x, so the consume above is a last use
  use(x)    // valid: reads the reassigned value
}

func varUseBeforeInit(_ b: Bool) {
  var x: C // expected-note {{variable defined here}}
  if b { x = C() }
  use(x) // expected-error {{variable 'x' used before being initialized}}
}

// The explicit `consume` operator on a copyable value. We diagnose it as an error.
func consumeOperatorCopyable() {
  let x: C
  x = C()
  _ = consume x // expected-note {{consumed here}}
  use(x)    // expected-error {{'x' used after consume}}
}

// In contrast to the above, simply having a consuming use means we should implicitly copy it.
func copyableConsumingUse() {
  let x: C
  x = C()
  take(x)
  use(x)
}

func consumeOfCopyableLet() {
  let p = C()
  _ = consume p  // expected-note {{consumed here}}
  _ = p          // expected-error {{'p' used after consume}}
}

func consumeOfCopyableLetIntoBinding() {
  let p = C()
  let q = consume p  // expected-note {{consumed here}}
  _ = p              // expected-error {{'p' used after consume}}
  use(q)
}

// --- simple ~Copyable (loadable), backed by alloc_box ---

func ncUseAfterConsume() {
  let x: NC
  x = NC(c: C())
  takeNC(x)  // expected-note {{consumed here}}
  useNC(x)   // expected-error {{'x' used after consume}}
}

func ncBorrowThenConsume() {
  let x: NC
  x = NC(c: C())
  useNC(x)   // borrow
  takeNC(x)  // valid: last use, consumes ownership
}

func ncUseBeforeInit(_ b: Bool) {
  let x: NC // expected-note {{constant defined here}}
  if b { x = NC(c: C()) }
  useNC(x)   // expected-error {{constant 'x' used before being initialized}}
}

func ncVarReinitAfterConsume() {
  var x: NC
  x = NC(c: C())
  takeNC(x)       // consume
  x = NC(c: C())  // reinitialize
  useNC(x)        // valid: reads the reinitialized value
}

func ncConsumeOnOneBranchUninitOnOther(_ b: Bool) {
  let x: NC // expected-note {{constant defined here}}
  if b {
    x = NC(c: C())
    takeNC(x)  // expected-note {{consumed here}}
    useNC(x)   // expected-error {{'x' used after consume}}
  } else {
    useNC(x)   // expected-error {{constant 'x' used before being initialized}}
  }
}

// Consumed on only one path, then used at the merge: an error on the `b` path.
func ncConditionalConsumeThenUse(_ b: Bool) {
  let x: NC
  x = NC(c: C())
  if b {
    takeNC(x)  // expected-note {{consumed here}}
  }
  useNC(x)     // expected-error {{'x' used after consume}}
}

// Consumed as the last use on every path, with no use after the merge: valid.
func ncConsumePerBranch(_ b: Bool) {
  let x: NC
  x = NC(c: C())
  if b {
    takeNC(x)  // last use on this path
  } else {
    useNC(x)   // borrow
    takeNC(x)  // last use on this path
  }
}

// --- the explicit `consume` operator on a ~Copyable value ---

// `consume x` ends x's lifetime; using it afterwards is an error.
func ncConsumeOperatorThenUse() {
  let x: NC
  x = NC(c: C())
  _ = consume x       // expected-note {{consumed here}}
  useNC(x)            // expected-error {{'x' used after consume}}
}

// Binding the consumed value: still a use-after-consume of x; the new binding is fine.
func ncConsumeOperatorLetBinding() {
  let x: NC
  x = NC(c: C())
  let y = consume x   // expected-note {{consumed here}}
  useNC(x)            // expected-error {{'x' used after consume}}
  useNC(y)
}

// `consume` then reinitialize a `var`: valid, x is live again at the use.
func ncConsumeOperatorReinit() {
  var x: NC
  x = NC(c: C())
  _ = consume x       // consume
  x = NC(c: C())      // reinitialize
  useNC(x)            // valid
}

// `consume` as the genuine last use: valid.
func ncConsumeOperatorLastUse() {
  let x: NC
  x = NC(c: C())
  useNC(x)
  _ = consume x       // valid: last use
}

// --- box-backed ~Copyable locals initialized with a value (no mark_uninitialized) ---

// A ~Copyable `let` initialized inline is backed by an `alloc_box` (with an
// initial `store [init]` and no `mark_uninitialized`); the double consume must
// still be diagnosed.
func ncInitializedLetDoubleConsume() {
  let x = NC(c: C())
  takeNC(x)  // expected-note {{consumed here}}
  takeNC(x)  // expected-error {{'x' used after consume}}
}

func ncInitializedLetConsumeOperator() {
  let x = NC(c: C())
  _ = consume x       // expected-note {{consumed here}}
  useNC(x)            // expected-error {{'x' used after consume}}
}

// A `consuming` parameter is likewise backed by an `alloc_box` initialized with
// the incoming argument, with no `mark_uninitialized`.
func ncConsumingParamDoubleConsume(_ x: consuming NC) {
  takeNC(x)  // expected-note {{consumed here}}
  takeNC(x)  // expected-error {{'x' used after consume}}
}

// --- field sensitivity (copyable aggregates) ---
//
// Demand is tracked per leaf subelement, so consuming one field does not affect
// another. Consuming `p.a` leaves `p.b` live (a whole-value analysis would instead
// reject the use of `p.b`), and reusing the *same* consumed field is diagnosed as a
// use-after-consume that names the binding and points at the offending later use.

struct Pair { var a: C; var b: C }

func fieldConsumeThenUseOther() {
  let p = Pair(a: C(), b: C())
  _ = consume p.a
  use(p.b)   // valid: distinct field
}

func fieldUseOtherThenConsume() {
  let p = Pair(a: C(), b: C())
  use(p.b)
  _ = consume p.a   // valid: last use of a, b already read
}

func tupleElementConsumeThenUseOther() {
  let t = (C(), C())
  _ = consume t.0
  use(t.1)   // valid: distinct element
}

func fieldUseAfterConsume() {
  var p: Pair
  p = Pair(a: C(), b: C())
  _ = consume p.a   // expected-note {{consumed here}}
  use(p.a)          // expected-error {{'p' used after consume}}
  use(p.b)          // valid: distinct field
}

func tupleElementUseAfterConsume() {
  var t: (C, C)
  t = (C(), C())
  _ = consume t.0   // expected-note {{consumed here}}
  use(t.0)          // expected-error {{'t' used after consume}}
  use(t.1)          // valid: distinct element
}

// FIXME: this case is still missing diagnostics!
func consumeCopyableFields() {
  let p = Pair(a: C(), b: C())
  _ = consume p.a
  _ = p.a
}


func testStringSwitch(_ s: String) -> Int {
  switch s {
  case "Swift": return 0
  case "C++": return 1
  default: return 99
  }
}

func testArray(_ s: Array<String>) -> String {
  return s[0]
}
