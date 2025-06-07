@unsafe public struct PointerType { } // expected-note{{'PointerType' declared here}}

public typealias UnsafeTypeAlias = PointerType

public func getPointers() -> [PointerType] { [] }

public struct HasAPointerType {
  public typealias Ptr = PointerType
}

public protocol Ptrable {
  associatedtype Ptr
}

extension HasAPointerType: Ptrable { }

public protocol HasUnsafeRequirement {
  func f(_: PointerType)
}

open class SuperclassWithUnsafeMethod {
  open func implicitlyUnsafe(_: PointerType) { }
}
