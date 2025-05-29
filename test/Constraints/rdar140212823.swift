// RUN: %target-swift-emit-silgen %s -verify -swift-version 6

// rdar://140212823 - Make sure we build curry thunks using the adjusted
// reference type, such that the ParenExpr agrees with the type.

class C: @unchecked Sendable {
  func foo() {}
}
class D: C, @unchecked Sendable {
  func bar() {
    let _ = (super.foo)
  }
}

struct S {
  func instanceMethod() {}
  func foo() {
    let _ = (self.instanceMethod)
  }
  static func staticMethod() {}
}

let _ = (S.instanceMethod)
let _ = (type(of: S()).instanceMethod)

let _ = (S.staticMethod)
let _ = (type(of: S()).staticMethod)

let _: (Int, Int) -> Int = (+)
