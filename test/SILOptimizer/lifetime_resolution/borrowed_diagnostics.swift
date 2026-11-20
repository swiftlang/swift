// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify %s -o /dev/null

// Diagnostics from LifetimeResolution for borrowed values that are consumed.

class C {}
struct S { var c: C }
struct NC: ~Copyable {
  var c: C
  func f() {}
}
struct Inner: ~Copyable {}
struct Outer: ~Copyable { var a: Inner; var b: Inner }
enum E: ~Copyable { case a(NC), b }

func takeC(_ c: consuming C) {}
func useC(_ c: borrowing C) {}
func takeS(_ s: consuming S) {}
func takeNC(_ x: consuming NC) {} // #takeNC
func useNC(_ x: borrowing NC) {}
func takeInner(_ x: consuming Inner) {} // #takeInner
func mk() -> Outer { Outer(a: Inner(), b: Inner()) }

//===----------------------------------------------------------------------===//
// Borrowed noncopyable values
//===----------------------------------------------------------------------===//

func ncMethod(_ n: borrowing NC) { n.f() }
func ncUse(_ n: borrowing NC) { useNC(n) }
func ncField(_ n: borrowing NC) { takeC(n.c) }

func ncConsume(_ n: borrowing NC) { // expected-note {{make 'n' a 'consuming' parameter to take ownership of it}} {{21-30=consuming}}
  takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
  // expected-note@#takeNC {{consider making this parameter 'borrowing' if 'takeNC' does not need ownership of it}} {{18-27=borrowing}}
}

func ncConsumeTwice(_ n: borrowing NC) { // expected-note {{make 'n' a 'consuming' parameter}}
  takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
  // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
}

func ncPartial(_ o: borrowing Outer) { // expected-note {{make 'o' a 'consuming' parameter}}
  takeInner(o.a) // expected-error {{'o.a' is borrowed and cannot be consumed}}
  // expected-note@#takeInner {{consider making this parameter 'borrowing' if 'takeInner' does not need ownership of it}}
}

func ncLet(_ n: borrowing NC) { // expected-note {{make 'n' a 'consuming' parameter}}
  let x = n // expected-error {{'n' is borrowed and cannot be consumed}}
  useNC(x)
}

func ncReturn(_ n: borrowing NC) -> NC { // expected-note {{make 'n' a 'consuming' parameter}}
  return n // expected-error {{'n' is borrowed and cannot be consumed}}
}

func genericNC<T: ~Copyable>(_ t: borrowing T) -> T { // expected-note {{make 't' a 'consuming' parameter}} {{35-44=consuming}}
  return t // expected-error {{'t' is borrowed and cannot be consumed}}
}

func ncConsumeOperator(_ n: borrowing NC) { // expected-note {{make 'n' a 'consuming' parameter}}
  let x = consume n // expected-error {{'n' is borrowed and cannot be consumed}}
  useNC(x)
}

func ncConsumeInLoop(_ n: borrowing NC) { // expected-note {{make 'n' a 'consuming' parameter}}
  for _ in 0..<3 {
    takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }
}

func ncEnumPayload(_ e: borrowing E) {
  switch e {
  case .a(let n):
    takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  case .b:
    break
  }
}

extension NC {
  func consumeSelf() { // expected-note {{make 'consumeSelf()' a 'consuming' method to take ownership of 'self'}}
    takeNC(self) // expected-error {{'self' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }

  borrowing func consumeBorrowingSelf() { // expected-note {{make 'consumeBorrowingSelf()' a 'consuming' method}}
    takeNC(self) // expected-error {{'self' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }
}

struct Wrapper: ~Copyable {
  var inner: Inner

  func consumeField() { // expected-note {{make 'consumeField()' a 'consuming' method}}
    takeInner(self.inner) // expected-error {{'self.inner' is borrowed and cannot be consumed}}
    // expected-note@#takeInner {{consider making this parameter 'borrowing'}}
  }
}

//===----------------------------------------------------------------------===//
// Borrowed Copyable values
//===----------------------------------------------------------------------===//

func cExplicit(_ c: borrowing C) { takeC(copy c) }
func cUse(_ c: borrowing C) { useC(c) }
func cField(_ s: borrowing S) { takeC(s.c) }

func cConsume(_ c: borrowing C) {
  takeC(c) // expected-error {{'c' is borrowed and cannot be consumed without an explicit 'copy'}}
  // expected-note@-1 {{write 'copy c' to consume a copy instead}} {{9-9=copy }}
}

func cLet(_ c: borrowing C) {
  let x = c // expected-error {{'c' is borrowed and cannot be consumed without an explicit 'copy'}}
  // expected-note@-1 {{write 'copy c' to consume a copy instead}} {{none}}
  useC(x)
}

func cWhole(_ s: borrowing S) {
  takeS(s) // expected-error {{'s' is borrowed and cannot be consumed without an explicit 'copy'}}
  // expected-note@-1 {{write 'copy s' to consume a copy instead}} {{9-9=copy }}
}

func generic<T>(_ t: borrowing T) -> T {
  return t // expected-error {{'t' is borrowed and cannot be consumed without an explicit 'copy'}}
  // expected-note@-1 {{write 'copy t' to consume a copy instead}} {{none}}
}

func cConsumeOperator(_ c: borrowing C) {
  let x = consume c // expected-error {{'c' is borrowed and cannot be consumed without an explicit 'copy'}}
  // expected-note@-1 {{write 'copy c' to consume a copy instead}} {{none}}
  useC(x)
}

struct CopyableWithMethods {
  var c: C
  consuming func eat() {}

  borrowing func eatImplicitSelf() {
    eat() // expected-error {{'self' is borrowed and cannot be consumed without an explicit 'copy'}}
    // expected-note@-1 {{write 'copy self' to consume a copy instead}} {{none}}
  }

  borrowing func eatExplicitSelf() {
    self.eat() // expected-error {{'self' is borrowed and cannot be consumed without an explicit 'copy'}}
    // expected-note@-1 {{write 'copy self' to consume a copy instead}} {{none}}
  }
}

//===----------------------------------------------------------------------===//
// Closure captures of borrowed values
//===----------------------------------------------------------------------===//

func callNonEscaping(_ f: () -> ()) { f() }

func captureNC(_ n: borrowing NC) {
  callNonEscaping { useNC(n) }
}

func captureAndConsumeNC(_ n: borrowing NC) {
  callNonEscaping { // expected-note {{'n' is captured by this closure, which only borrows it}}
    takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }
}

func captureAndConsumeC(_ c: borrowing C) {
  callNonEscaping { // expected-note {{'c' is captured by this closure, which only borrows it}}
    takeC(c) // expected-error {{'c' is borrowed and cannot be consumed without an explicit 'copy'}}
    // expected-note@-1 {{write 'copy c' to consume a copy instead}} {{11-11=copy }}
  }
}

func captureAndCopyC(_ c: borrowing C) {
  callNonEscaping { takeC(copy c) }
}

func captureLocalAndConsumeNC() {
  let n = NC(c: C())
  callNonEscaping { // expected-note {{'n' is captured by this closure, which only borrows it}}
    takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }
}

func escapingCaptureNC(_ n: borrowing NC) -> () -> () { // expected-note {{make 'n' a 'consuming' parameter}}
  return { useNC(n) } // expected-error {{borrowed 'n' cannot be captured by an escaping closure}}
}

func escapingCaptureC(_ c: borrowing C) -> () -> () {
  return { useC(c) } // expected-error {{borrowed 'c' cannot be captured by an escaping closure}}
}

//===----------------------------------------------------------------------===//
// Non-escaping closure captures converted to another function type
//===----------------------------------------------------------------------===//

func callRethrowing(_ f: () throws -> ()) rethrows { try f() }
class Base {}
class Derived: Base {}
func callContravariant(_ f: (Derived) -> ()) {}

func localFuncThrowsNC(_ n: borrowing NC) {
  func g() { useNC(n) }
  callRethrowing(g)
}

func localFuncThrowsC(_ c: borrowing C) {
  func g() { useC(c) }
  callRethrowing(g)
}

func localFuncNestedNC(_ n: borrowing NC) {
  callNonEscaping {
    func g() { useNC(n) }
    callRethrowing(g)
  }
}

func localFuncSubtypeC(_ c: borrowing C) {
  func g(_ x: Base) { useC(c) }
  callContravariant(g)
}

func localFuncConsumeNC(_ n: borrowing NC) {
  func g() { // expected-note {{'n' is captured by this local function, which only borrows it}}
    takeNC(n) // expected-error {{'n' is borrowed and cannot be consumed}}
    // expected-note@#takeNC {{consider making this parameter 'borrowing'}}
  }
  g()
}

//===----------------------------------------------------------------------===//
// Closure captures passed to coroutine accessors
//===----------------------------------------------------------------------===//

struct Holder {
  var v = 0
  subscript(read f: () -> ()) -> Int { _read { f(); yield v } }
  subscript(mod f: () -> ()) -> Int {
    get { f(); return v }
    _modify { f(); yield &v }
  }
}

func readNC(_ n: borrowing NC, _ h: Holder) -> Int { // expected-note {{make 'n' a 'consuming' parameter}}
  return h[read: { useNC(n) }] // expected-error {{borrowed 'n' cannot be captured by an escaping closure}}
}

func modifyC(_ c: borrowing C, _ h: inout Holder) {
  h[mod: { useC(c) }] = 3 // expected-error {{borrowed 'c' cannot be captured by an escaping closure}}
}
