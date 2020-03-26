@_exported import HasOverlay

extension Base {
  public struct NestedFromSwift {}
  public struct NestedAndShadowed {
    init(dummy: ()) {}
  }
}

public var shadowedFromSwift = Base.NestedAndShadowed(dummy: ())

public struct CustomError {
  public struct Code : RawRepresentable {
    public let rawValue: Int

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }
  }
}
