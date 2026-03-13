
public typealias Second<T, U> = U

public struct Packed<each T, U> {
  public var tuple1: (repeat each T)
  public var tuple2: (repeat (each T) -> ())
  public var tuple3: (repeat Scalar<each T>)
  public var tuple4: (repeat Second<each T, Int>)

  public var func1: (repeat each T) -> ()
  public var func2: (repeat Scalar<each T>) -> ()
  public var func3: (repeat Second<each T, Int>) -> ()

  public var nominal1: AlsoPacked<repeat each T>
  public var nominal2: AlsoPacked<repeat Scalar<each T>>
  public var nominal3: AlsoPacked<repeat Second<each T, Int>>
}

public struct AlsoPacked<each T> {
  public var t: (repeat each T)
}

public struct Scalar<T> {
  public var t: T
}

public protocol P {
  associatedtype A
  var t: A { get }
}

extension AlsoPacked: P {}

public struct NestedPacked<each T> {
  public struct Inner<each U> {
    var t: (repeat Packed<repeat each U, each T>)
    var u: (repeat AlsoPacked<repeat each U, each T>)
  }
}

public struct Simple {
  var x1: AlsoPacked<Float>
  var x2: AlsoPacked<Int, Float>
  var x3: AlsoPacked<Int, String, Float>
}

public struct Complex {
  var x1: Packed<Float>
  var x2: Packed<Int, Float>
  var x3: Packed<Int, String, Float>
}