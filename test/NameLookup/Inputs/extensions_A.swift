public struct X { }

public protocol P { }

public struct Y<T> { }

extension Y: P where T: P { }

public struct Z: P { }

infix operator <<<
infix operator >>>
infix operator <>

extension X {
  public func XinA() { }

  public static func <<<(a: Self, b: Self) -> Self { a }
}

extension Y {
  public func YinA() { }

  public static func <<<(a: Self, b: Self) -> Self { a }
}
