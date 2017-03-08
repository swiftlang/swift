import has_alias
@_exported import struct_with_operators

public func numeric(_ x: MyInt64) {}
public func conditional(_ x: AliasWrapper.Boolean) {}
public func conditional2(_ x: ProtoWrapper.Boolean) {}
public func longInt(_ x: Int.EspeciallyMagicalInt) {}

public func numericArray(_ x: IntSlice) {}


public protocol ExtraIncrementable {
  prefix func +++(base: inout Self)
}

extension SpecialInt : ExtraIncrementable {}

public protocol DefaultInitializable {
  init()
}

extension SpecialInt : DefaultInitializable {}
