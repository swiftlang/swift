
public struct ResilientStruct {
  public static var staticStoredProperty: Int8 = 0
  public var storedProperty: Int8 = 0
  public init() {}
}

@_fixed_layout
public struct FragileStruct {
  @_fixed_layout
  public static var staticStoredProperty: Int8 = 0
  public var storedProperty: Int8 = 0
  public init() {}
}

public final class ResilientFinalClass {
  public var storedProperty: Int8 = 0
  public init() {}
}

@_fixed_layout
public final class FragileFinalClass {
  public var storedProperty: Int8 = 0
  public init() {}
}

public var globalResilient: Int8 = 0

@_fixed_layout
public var globalFragile: Int8 = 0
