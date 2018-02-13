public class A<T> {
  typealias Element = T
  @_versioned
  @_inlineable
  func convertFromArrayLiteral(_ elements: Element...) -> A {
    return A()
  }

  @_versioned
  @_inlineable
  init() {}

  @_inlineable public subscript<U>(value: T) -> U? {
    return nil
  }
}
