// RUN: not %target-swift-emit-silgen %s 2>&1 | %FileCheck %s

protocol P {
  static var foo: Int { get }
}

struct S {
  init(_ kp: KeyPath<P.Type, Int>) {}
}

func test() {
  S(\.foo)
}

// CHECK: error: key path cannot refer to static member 'foo' of protocol type 'P'
