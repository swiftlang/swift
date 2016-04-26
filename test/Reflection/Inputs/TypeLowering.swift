public struct BasicStruct {
  public let i1: Int8
  public let i2: Int16
  public let i3: Int32

  public let bi1: Box<Int8>
  public let bi2: Box<Int16>
  public let bi3: Box<Int32>
}

public struct Box<T> {
  public let value: T
}

public protocol P {
  associatedtype A
  associatedtype B
}

public struct Foo<T, U> : P {
  public typealias A = Box<U>
  public typealias B = Box<T>
}

public struct Bar<T : P> {
  public let a: T.A
  public let b: T.B
  public let c: (T.A, T.B)
}

public struct AssocTypeStruct {
  public let t: Bar<Foo<Int8, Int16>>
}

public class C {}

public struct ReferenceStruct {
  public let strongRef: C
  public let strongOptionalRef: C
  public unowned let unownedRef: C
  public weak var weakRef: C?
  public unowned(unsafe) let unmanagedRef: C
}

public struct FunctionStruct {
  public let thickFunction: () -> ()
  public let thinFunction: @convention(thin) () -> ()
  public let cFunction: @convention(c) () -> ()
}

public protocol P1 {}
public protocol P2 : P1 {}
public protocol P3 {}

public protocol CP1 : class {}
public protocol CP2 : CP1 {}

public struct ExistentialStruct {
  public let any: Any
  public let anyObject: AnyObject
  public let anyProto: P1
  public let anyProtoComposition: protocol<P1, P2, P3>
  public let anyClassBoundProto1: CP1
  public let anyClassBoundProto2: CP2
  public let anyClassBoundProtoComposition1: protocol<CP1, CP2>
  public let anyClassBoundProtoComposition2: protocol<P1, CP2>

  public weak var weakAnyObject: AnyObject?
  public weak var weakAnyClassBoundProto: CP1?
}

public struct MetadataHolder<T, U> {
  let t: T
  let u: U.Type
}

public struct MetatypeStruct {
  public let any: Any.Type
  public let anyObject: AnyObject.Type
  public let anyProto: P1.Type
  public let anyProtoComposition: protocol<P1, P2, P3>.Type
  public let structMetatype: BasicStruct.Type
  public let classMetatype: C.Type
  public let abstractMetatype: MetadataHolder<BasicStruct.Type, BasicStruct>
}
