// RUN: %target-swift-frontend -emit-silgen %s

// This is a SILGen test to ensure we can completely check these conformances
// and build valid AST.

protocol P {
  associatedtype T : Q = S
  typealias Y = T.X

  func foo(_: T.X)
}

protocol Q {
  associatedtype X
}

struct S : Q {
  typealias X = ()
}

struct R : P {
  let x: Y? = nil
  func foo(_: Y) {}
}

// SR-8813
protocol BaseProtocol {
  associatedtype Value
  typealias Closure = () -> Value

  init(closure: Closure)
}

struct Base<Value>: BaseProtocol {
  private var closure: Closure?

  init(closure: Closure) {
    withoutActuallyEscaping(closure) { new in
      self.closure = new
    }
  }
}

// SR-11407
protocol _Drivable: AnyObject {
  typealias Driver = Self
}
protocol Configurator {
  associatedtype Drivable: _Drivable
  typealias Driver = Drivable.Driver
  func configure(driver: Driver)
}
struct AnyConfigurator<Drivable: _Drivable>: Configurator {
  private let thing: Driver?
  func configure(driver: AnyConfigurator.Driver) {}
}
