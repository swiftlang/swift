public struct StructFromIndirect {
  public init() {}
}
public typealias AliasFromIndirect = StructFromIndirect
public typealias GenericAliasFromIndirect<T> = (StructFromIndirect, T)

public func globalFunctionFromIndirect() {}
public var globalVariableFromIndirect = 0
