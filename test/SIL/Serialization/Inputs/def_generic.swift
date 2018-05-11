@_fixed_layout
public class A<T> {
  @usableFromInline typealias Element = T

  @inlinable func convertFromArrayLiteral(_ elements: Element...) -> A {
    return A()
  }

  @inlinable init() {}

  @inlinable public subscript<U>(value: T) -> U? {
    return nil
  }
}
