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

public protocol Q : P {}

public struct ConformsP<T, U> : P {
  public typealias A = Box<U>
  public typealias B = Box<T>
}

public struct ConformsQ<T, U> : Q {
  public typealias A = Box<U>
  public typealias B = Box<T>
}

public class Base<T, U> : P {
  public typealias A = Box<T>
  public typealias B = Box<U>
}

public class Derived : Base<Int8, Int16> {}

public class GenericDerived<T> : Base<T, T> {}

public struct Bar<T : P> {
  public let a: T.A
  public let b: T.B
  public let c: (T.A, T.B)
}

public struct AssocTypeStruct {
  public let t1: Bar<ConformsP<Int8, Int16>>
  public let t2: Bar<ConformsQ<Int8, Int16>>
  public let t3: Bar<Base<Int8, Int16>>
  public let t4: Bar<Derived>
  public let t5: Bar<GenericDerived<Int8>>
}

public class C {}

public struct ReferenceStruct {
  public let strongRef: C
  public let optionalStrongRef: C?

  public let strongRefTuple: (C, C)
  public let optionalStrongRefTuple: (C, C)?

  public unowned let unownedRef: C

  public weak var weakRef: C?

  public unowned(unsafe) let unmanagedRef: C
}

public struct FunctionStruct {
  public let thickFunction: () -> ()
  public let optionalThickFunction: (() -> ())?

  public let thinFunction: @convention(thin) () -> ()
  public let optionalThinFunction: (@convention(thin) () -> ())?

  public let cFunction: @convention(c) () -> ()
  public let optionalCFunction: (@convention(c) () -> ())?
}

public protocol P1 {}
public protocol P2 : P1 {}
public protocol P3 {}

public protocol CP1 : class {}
public protocol CP2 : CP1 {}

public struct ExistentialStruct {
  public let any: Any
  public let optionalAny: Any?

  public let anyObject: AnyObject
  public let optionalAnyObject: AnyObject?

  public let anyProto: P1
  public let optionalAnyProto: P1?

  public let anyProtoComposition: P1 & P2 & P3
  public let optionalAnyProtoComposition: (P1 & P2 & P3)?

  public let anyClassBoundProto1: CP1
  public let optionalAnyClassBoundProto1: CP1?

  public let anyClassBoundProto2: CP2
  public let optionalAnyClassBoundProto2: CP2?

  public let anyClassBoundProtoComposition1: CP1 & CP2
  public let optionalAnyClassBoundProtoComposition1: (CP1 & CP2)?

  public let anyClassBoundProtoComposition2: P1 & CP2
  public let optionalAnyClassBoundProtoComposition2: (P1 & CP2)?
  
  public weak var weakAnyObject: AnyObject?
  public weak var weakAnyClassBoundProto: CP1?

  public let classConstrainedP1: C & P1
}

public struct MetadataHolder<T, U> {
  let t: T
  let u: U.Type
}

public struct MetatypeStruct {
  public let any: Any.Type
  public let optionalAny: Any.Type?

  public let anyObject: AnyObject.Type
  public let optionalAnyObject: AnyObject.Type?

  public let anyProto: P1.Type
  public let optionalAnyProto: P1.Type?

  public let anyProtoComposition: (P1 & P2 & P3).Type
  public let optionalAnyProtoComposition: (P1 & P2 & P3).Type?

  public let structMetatype: BasicStruct.Type
  public let optionalStructMetatype: BasicStruct.Type?

  public let classMetatype: C.Type
  public let optionalClassMetatype: C.Type?

  public let abstractMetatype: MetadataHolder<BasicStruct.Type, BasicStruct>
}

// We don't allow stored properties to have uninhabited types now, but make a
// wrapper over one to continue testing this 
public enum EmptyEnum {}

public struct EmptyEnumWrapper<T> {
  public var value: T
}

public enum NoPayloadEnum {
  case A
  case B
  case C
  case D
}

public enum SillyNoPayloadEnum {
  case A(EmptyEnum)
  case B(EmptyEnum)
  case C
  case D
}

public enum SingletonEnum {
  case Payload(C)
}

public enum SinglePayloadEnum {
  indirect case Indirect(Any)
  case Nothing
}

public enum MultiPayloadConcrete {
  case Left(C)
  case Right(C)
  case Donkey
  case Mule
  case Horse
}

public enum MultiPayloadGenericFixed<T : C> {
  case Left(T)
  case Right(T)
  case Donkey
  case Mule
  case Horse
}

public enum MultiPayloadGenericDynamic<T, U> {
  case Left(T)
  case Right(U)
  case Donkey
  case Mule
  case Horse
}

public struct EnumStruct {
  public let empty: EmptyEnumWrapper<EmptyEnum>
  public let noPayload: NoPayloadEnum
  public let sillyNoPayload: SillyNoPayloadEnum
  public let singleton: SingletonEnum
  public let singlePayload: SinglePayloadEnum

  public let multiPayloadConcrete: MultiPayloadConcrete
  public let multiPayloadGenericFixed: MultiPayloadGenericFixed<C>
  public let multiPayloadGenericDynamic: MultiPayloadGenericDynamic<Int8, Int>

  // Double-optional class reference does not need
  // any extra storage
  public let optionalOptionalRef: C??

  // Double-optional raw pointer needs an extra
  // tag byte
  public let optionalOptionalPtr: UnsafePointer<Int>??
}
