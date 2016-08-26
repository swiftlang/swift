public extension Array {
  func wobble() -> Element? { return nil }
}

public protocol P {
  var property: Int { get }
}

public protocol Q { }

extension P {
  public var property: Int { return 0 }
}

extension P where Self: Q {
  public var property: Int { return 0 }
}

public struct ConformsToP: P { }




