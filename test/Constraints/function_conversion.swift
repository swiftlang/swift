// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

// rdar://problem/31969605

class Base {}
class Derived : Base {}

protocol Refined {}
protocol Proto : Refined {}
extension Base : Refined {}

func baseFn(_: Base) {}

func superclassConversion(fn: @escaping (Base) -> ()) {
  let _: (Derived) -> () = fn
}

func existentialConversion(fn: @escaping (Refined) -> ()) {
  let _: (Proto) -> () = fn
  let _: (Base) -> () = fn
  let _: (Derived) -> () = fn
}
