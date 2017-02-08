// RUN: %target-typecheck-verify-swift -swift-version 3

// Swift 3 used default literal types even for normal protocol constraints,
// which led to nonsensical type inference behavior.

func f<T: ExpressibleByIntegerLiteral>(_: T = 0) { }
f()

struct X<T: ExpressibleByIntegerLiteral> {
  func g() { }
}

X().g()
