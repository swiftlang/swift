import Foundation

@inline(__always) @_transparent public func compareToSelf<T: Equatable>(_ t: T) -> Bool {
  return t == t
}

@inline(__always) @_transparent public func compareImportedEnumToSelf(_ e: NSRuncingMode) -> Bool {
  return compareToSelf(e)
}

// SR-6105
open class EVC<EnumType : EVT> where EnumType.RawValue : Hashable {
}

public protocol EVT : RawRepresentable {
    associatedtype Wrapper
    associatedtype Constructor
}

open class EVO<EnumType> {
}

final public class CSWrapperConstructorClass : EVC<ByteCountFormatter.CountStyle> {
}

final public class CSWrapperClass : EVO<ByteCountFormatter.CountStyle> {
}
extension ByteCountFormatter.CountStyle : EVT {
    public typealias Wrapper = CSWrapperClass
    public typealias Constructor = CSWrapperConstructorClass
}
