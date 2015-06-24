// RUN: %target-swift-frontend %s -emit-ir

// rdar://20985062

class A {
  func foo(a: [(x: Int, y: Double)]) {}
}

func accept<T>(t: T.Type, inout _ value: T) {}

typealias TheType = (A) -> ([(x: Int, y: Double)]) -> ()
var curried = A.foo
accept(TheType.self, &curried)

