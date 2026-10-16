// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -verify %s

// Use-after-consume diagnostics from LifetimeResolution. Each error is reported at
// the offending use, with a note at the consume.

class C {}
func use(_ c: C) {}
func take(_ c: __owned C) {}

struct NC: ~Copyable {
  var c: C
  consuming func eat() {}
  borrowing func look() {}
}
func takeNC(_ x: consuming NC) {}
func useNC(_ x: borrowing NC) {}
func takeAndUse(_ a: consuming NC, _ b: borrowing NC) {}
func takeGeneric<T: ~Copyable>(_ t: consuming T) {}
func useGeneric<T: ~Copyable>(_ t: borrowing T) {}

func usedAfterConsume() {
  let x = NC(c: C())
  takeNC(x) // expected-note {{consumed here}}
  useNC(x) // expected-error {{'x' used after consume}}
}

func consumedTwice() {
  let x = NC(c: C())
  takeNC(x) // expected-note {{consumed here}}
  takeNC(x) // expected-error {{'x' consumed more than once}}
}

func consumedInLoop(_ b: Bool) {
  let x = NC(c: C())
  while b {
    takeNC(x) // expected-error {{'x' consumed in a loop}}
  }
}

func consumedAndUsedAtOnce() {
  let x = NC(c: C())
  takeAndUse(x, x) // expected-note {{consumed here}}
                   // expected-error@-1 {{'x' consumed and used at the same time}}
}

func consumedOnOneBranch(_ b: Bool) {
  let x = NC(c: C())
  if b {
    takeNC(x) // expected-note {{consumed here}}
  }
  useNC(x) // expected-error {{'x' used after consume}}
}

func consumedByMethod() {
  let x = NC(c: C())
  x.eat() // expected-note {{consumed here}}
  x.look() // expected-error {{'x' used after consume}}
}

func consumeOperator() {
  let x = NC(c: C())
  _ = consume x // expected-note {{consumed here}}
  useNC(x) // expected-error {{'x' used after consume}}
}

func consumedGeneric<T: ~Copyable>(_ t: consuming T) {
  takeGeneric(t) // expected-note {{consumed here}}
  useGeneric(t) // expected-error {{'t' used after consume}}
}

func inoutNotReinitialized(_ x: inout NC) {
  takeNC(x) // expected-error {{missing reinitialization of inout parameter 'x' after consume}}
}

func inoutReinitialized(_ x: inout NC) {
  takeNC(x)
  x = NC(c: C())
}

func inoutConsumeOperator(_ s: inout String) {
  if .random() {
    _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  }
}

// --- inout parameters that are not reinitialized when an error is thrown ---

struct E: Error {}
func mayThrow() throws {}

struct ArrayBuffer {
  var pending: [UInt8] = []

  mutating func flush(to sink: (consuming [UInt8]) throws -> Void) rethrows {
    let bytes = consume pending // expected-error {{missing reinitialization of inout parameter 'self.pending' after consume}}
    try sink(bytes)
    pending = []
  }
}

func inoutExplicitThrow(_ s: inout String, _ b: Bool) throws {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  if b { throw E() }
  s = ""
}

func inoutTypedThrow(_ s: inout String, _ b: Bool) throws(E) {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  if b {
    throw E()
  }
  s = ""
}

func inoutSeveralThrowingCalls(_ s: inout String) throws {
  defer { _ = 0 }
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  try mayThrow()
  try mayThrow()
  s = ""
}

func inoutRethrowFromCatch(_ s: inout String) throws {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  do {
    try mayThrow()
  } catch {
    throw E()
  }
  s = ""
}

func inoutCaughtError(_ s: inout String) throws {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  do {
    try mayThrow()
    s = ""
  } catch {}
}

func inoutReinitializedBeforeThrowing(_ s: inout String) throws {
  _ = consume s
  s = ""
  try mayThrow()
}

func inoutNoncopyableThrow(_ x: inout NC) throws {
  _ = consume x // expected-error {{missing reinitialization of inout parameter 'x' after consume}}
  try mayThrow()
  x = NC(c: C())
}

// --- inout parameters that are not reinitialized before an early return ---

extension ArrayBuffer {
  mutating func earlyReturn() {
    _ = consume pending // expected-error {{missing reinitialization of inout parameter 'self.pending' after consume}}
    guard .random() else { return }
    pending = []
  }
}

func inoutEarlyReturnValue(_ s: inout String, _ b: Bool) -> Int {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  if b {
    return 1
  }
  s = ""
  return 0
}

func inoutEarlyReturnWithDefer(_ s: inout String, _ b: Bool) {
  defer { _ = 0 }
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  if b { return }
  s = ""
}

func inoutFallsOffTheEnd(_ s: inout String, _ b: Bool) {
  _ = consume s // expected-error {{missing reinitialization of inout parameter 's' after consume}}
  if b { s = "" }
}

// --- copyable values with the explicit `consume` operator ---

func copyableUsedAfterConsume() {
  let x = C()
  _ = consume x // expected-note {{consumed here}}
  use(x) // expected-error {{'x' used after consume}}
}

func copyableConsumedIntoParameter() {
  let x = C()
  take(consume x) // expected-note {{consumed here}}
  use(x) // expected-error {{'x' used after consume}}
}

func copyableConsumedInLoop(_ b: Bool) {
  let x = C()
  while b {
    _ = consume x // expected-error {{'x' consumed in a loop}}
  }
}

func copyableCapturedAfterConsume() {
  var x = C()
  _ = consume x // expected-note {{consumed here}}
  let g = { use(x) } // expected-error {{'x' used after consume}}
  g()
  x = C()
}

func genericCopyable<T>(_ t: T) {
  let x = t
  _ = consume x // expected-note {{consumed here}}
  useGeneric(x) // expected-error {{'x' used after consume}}
}

// --- fields ---

struct Pair { var a: C; var b: C }

func fieldUsedAfterConsume() {
  var p: Pair
  p = Pair(a: C(), b: C())
  _ = consume p.a // expected-note {{consumed here}}
  use(p.a) // expected-error {{'p.a' used after consume}}
  use(p.b)
}

func tupleElementUsedAfterConsume() {
  var t: (first: C, second: C)
  t = (C(), C())
  _ = consume t.first // expected-note {{consumed here}}
  use(t.first) // expected-error {{'t.first' used after consume}}
}

func usedAfterPartialConsume() {
  var p: Pair
  p = Pair(a: C(), b: C())
  _ = consume p.a // expected-note {{partially consumed here}}
  let q = p // expected-error {{cannot use 'p' after partial consume}}
  _ = q
}

func fieldOfValueUsedAfterConsume() {
  let p = Pair(a: C(), b: C())
  _ = consume p // expected-note {{consumed here}}
  use(p.a) // expected-error {{'p' used after consume}}
}

struct NCPair: ~Copyable { var a: NC; var b: NC }

func ncConsumeOneFieldUseOther(_ p: consuming NCPair) {
  takeNC(p.a)
  useNC(p.b)
}

// --- consumes into other bindings ---

func copyableConsumedIntoBindingTwice() {
  let x = C()
  let a = consume x // expected-note {{consumed here}}
  let b = consume x // expected-error {{'x' consumed more than once}}
  use(a)
  use(b)
}
