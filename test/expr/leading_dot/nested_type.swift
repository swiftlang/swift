// RUN: %target-typecheck-verify-swift

// Nested subclass, typealias Self.
class Base {
  class Derived : Base {
    init(x: Int) {}
    class Sub: Derived {
      init(y: Int) {}
    }
    typealias Ident = Derived
    typealias Ident2 = Base
  }
  typealias Ident = Base
}

let _: Base = .Derived(x: 12)
let _: Base = .Ident()
let _: Base = .Derived.Sub(y: 1)
let _: Base = .Derived.init(x: 3)
let _: Base = .Derived.Ident(x: 3)
let _: Base = .Derived.Ident2()

// Typealias in protocol.
protocol P {
  typealias Impl1 = ConcreteP
}
extension P {
  typealias Impl2 = ConcreteP
}
struct ConcreteP : P {
  struct NestedP: P {}
  typealias Same = ConcreteP
}

let _: P = .Impl1()
let _: P = .Impl2()
let _: ConcreteP = .Impl1()
let _: ConcreteP = .Impl2()
let _: P = .Impl1.NestedP()
let _: P = .Impl2.NestedP()
let _: ConcreteP = .Impl1.Same()
let _: ConcreteP = .Impl2.Same()
let _: P = .Impl1.init()
let _: P = .Impl2.init()
let _: ConcreteP = .Impl1.init()
let _: ConcreteP = .Impl2.init()
