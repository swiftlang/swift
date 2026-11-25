// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -sil-verify-all -verify %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -disable-sil-opaque-values -sil-verify-all -verify %s -o /dev/null

// Valid consumes of existentials through their opened payload.

protocol P: ~Copyable {
  consuming func con()
  borrowing func bor()
  mutating func mut()
}
struct S: P, ~Copyable {
  consuming func con() {}
  borrowing func bor() {}
  mutating func mut() {}
}
func makeP() -> any P & ~Copyable { S() }
func callNonEscaping(_ f: () -> ()) { f() }

protocol Foo {
  var foo: String { get }
  consuming func eat()
}

func copyableExistentialLastUse(_ a: consuming any Foo) -> String {
  return a.foo
}

func copyableExistentialConsumedTwice(_ a: consuming any Foo) {
  a.eat()
  a.eat()
}

func existentialConsumingMethod(_ x: consuming any P & ~Copyable) {
  x.con()
}

func existentialBorrowMutateConsume(_ x: consuming any P & ~Copyable) {
  x.bor()
  x.mut()
  x.con()
}

func existentialConsumedOnOneBranch(_ x: consuming any P & ~Copyable, _ b: Bool) {
  if b {
    x.con()
  } else {
    x.bor()
  }
}

func existentialReinitialized(_ x: consuming any P & ~Copyable, _ y: consuming any P & ~Copyable) {
  x.con()
  x = y
  x.con()
}

func existentialInoutReinitialized(_ x: inout any P & ~Copyable, _ y: consuming any P & ~Copyable) {
  x.con()
  x = y
}

func existentialVarReinitializedInLoop() {
  var x = makeP()
  for _ in 0..<3 {
    x.con()
    x = makeP()
  }
  x.bor()
}

func existentialCaptureReinitialized() {
  var x = makeP()
  callNonEscaping {
    x.con()
    x = makeP()
  }
  x.bor()
}

struct PBox: ~Copyable {
  var e: any P & ~Copyable

  consuming func run() {
    e.con()
  }

  mutating func replace(_ n: consuming any P & ~Copyable) {
    e.con()
    e = n
  }
}

func existentialFieldConsumed(_ b: consuming PBox) {
  b.e.con()
}
