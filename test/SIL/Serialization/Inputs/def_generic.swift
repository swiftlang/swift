@_fixed_layout
public class A<T> {
  typealias Element = T
  @usableFromInline
  @inlinable
  func convertFromArrayLiteral(_ elements: Element...) -> A {
    return A()
  }

  @usableFromInline
  @inlinable
  init() {}

  @inlinable public subscript<U>(value: T) -> U? {
    return nil
  }
}
