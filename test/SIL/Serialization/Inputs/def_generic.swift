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
}
