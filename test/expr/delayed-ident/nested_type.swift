// RUN: %target-typecheck-verify-swift

// Nested subclass, typealias Self.
class Base {
  class Derived : Base {
    init(x: Int) {}
  }
  typealias Ident = Base
}

let _: Base = .Derived(x: 12)
let _: Base = .Ident()

// Typealias in protocol.
protocol P {
  typealias Impl1 = ConcreteP
}
extension P {
  typealias Impl2 = ConcreteP
}
struct ConcreteP : P {
}

let _: P = .Impl1()
let _: P = .Impl2()
let _: ConcreteP = .Impl1()
let _: ConcreteP = .Impl2()
