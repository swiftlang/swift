// Error cases for literal expressions in integer generic parameter values
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip

struct V<let N: Int, let M: Int> {}
// expected-note@-1 {{arguments to generic parameter 'N' ('2' and '5') are expected to be equal}}

extension V where N == 2 {
  // expected-note@-1 {{where 'N' = '3'}}
  func nIsTwo() {}
}

extension V where M == 6 {
  // expected-note@-1 {{where 'M' = '0'}}
  func mIsSix() {}
}

func testRequirementExprMatch() {
  let a: V<(1+1), 6> = V()
  a.nIsTwo() // OK
  a.mIsSix() // OK
}

struct S<let N: Int> {
  func foo() where S<N> == S<2> {} // OK
}

func testRequirementExprMismatch() {
  let b: V<(3 + 0), 0> = V()
  b.nIsTwo() // expected-error {{referencing instance method 'nIsTwo()' on 'V' requires the types '3' and '2' be equivalent}}
  b.mIsSix() // expected-error {{referencing instance method 'mIsSix()' on 'V' requires the types '0' and '6' be equivalent}}
}

func testRequirementExprToExprEquiv() {
  let c: V<(1 + 1), 0> = V()
  c.nIsTwo() // OK
}

func takesV2<let M: Int>(_ v: V<2, M>) {}
func takesV5<let M: Int>(_ v: V<5, M>) {}
func testExprArgPassedToConstrainedFunc() {
  let v: V<(1 + 1), 0> = V()
  takesV2(v)  // OK
  takesV5(v)  // expected-error {{cannot convert value of type 'V<2, 0>' to expected argument type 'V<5, 0>'}}
}

func foo(_: () -> Void) {}
var _: S<(foo() { // expected-error {{closures are not supported in generic value literal expressions}}
  struct Inner {}
  let inner = Inner()
})>

struct SRef<let N: Int> {
    func foo() where S<(N)> == S<(1+1)> {}
    // expected-note@-1 {{where 'N' = '13'}}
}
func testRequirementOnFoo() {
    let a: SRef<(2+0)> = SRef()
    a.foo() // OK
    let b: SRef<(2+11)> = SRef()
    b.foo() // expected-error {{instance method 'foo()' requires the types '13' and '2' be equivalent}}
}
