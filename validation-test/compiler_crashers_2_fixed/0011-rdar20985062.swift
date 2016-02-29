// RUN: %target-swift-frontend %s -emit-ir

// rdar://20985062

class A {
  func foo(a: [(x: Int, y: Double)]) {}
}

func accept<T>(t: T.Type, _ value: inout T) {}

typealias TheType = (A) -> ([(x: Int, y: Double)]) -> ()
var curried = A.foo
accept(TheType.self, &curried)

