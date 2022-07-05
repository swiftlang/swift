// RUN: %target-typecheck-verify-swift

struct S {
  func crash() -> Never {
    fatalError("")
  }
}

class A {
  func value() -> Int { 42 }
}

class B : A {
  let value: S = S()

  func test() throws -> B {
    value.crash() // Ok
  }
}
