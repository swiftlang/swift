@_exported import indirects

public struct StructFromDirect {
  public func method() {}
  public var property: Int {
    get { return 0 }
    set {}
  }
  public subscript(index: Int) -> Int {
    get { return 0 }
    set {}
  }
}
public typealias AliasFromDirect = StructFromDirect
public typealias GenericAliasFromDirect<T> = (StructFromDirect, T)

public func globalFunctionFromDirect() {}
public var globalVariableFromDirect = 0

extension StructFromIndirect {
  public func extensionMethodFromDirect() {}
  public var extensionPropertyFromDirect: Int {
    get { return 0 }
    set {}
  }
  public subscript(extensionSubscript index: Int) -> Int {
    get { return 0 }
    set {}
  }
}
