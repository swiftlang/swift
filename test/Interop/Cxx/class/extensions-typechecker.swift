// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import Extensions

extension Outer.Space.Foo {
  func bar() -> Int32 { return a }
}

extension Outer.Space.Foo: @retroactive ExpressibleByIntegerLiteral {
  public init(integerLiteral value: IntegerLiteralType) {
    self.init(a: Int32(value))
  }
}

var it: Outer.Space.Foo = 123
let res = it.bar()
