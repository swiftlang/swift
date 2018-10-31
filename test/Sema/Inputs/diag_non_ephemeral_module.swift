
public struct ResilientStruct {
  public var storedProperty: Int8 = 0
  public init() {}
}

@_fixed_layout
public struct FragileStruct {
  public var storedProperty: Int8 = 0
  public init() {}
}
