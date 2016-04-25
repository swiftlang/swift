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
