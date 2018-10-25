@_exported import HasOverlay

extension Base {
  public struct NestedFromSwift {}
  public struct NestedAndShadowed {
    init(dummy: ()) {}
  }
}

public var shadowedFromSwift = Base.NestedAndShadowed(dummy: ())
