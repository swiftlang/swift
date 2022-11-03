// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

public protocol P {}

struct S<T>: P {
  var x: Any
  init() { fatalError() }
}

public func mangleArchetype(_ p: any P) -> any P {
  p.open
}

extension P {
  var open: some P {
    S<Self>()
  }
}