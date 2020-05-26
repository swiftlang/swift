public protocol P {
  associatedtype Thing
  func foo() -> Thing
}

public struct AStruct<Thing>: P {
  public var thing: Thing
  public init(thing: Thing) {
    self.thing = thing
  }
  public func foo() -> Thing {
    return thing
  }
}
