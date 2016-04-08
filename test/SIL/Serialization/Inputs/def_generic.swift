class A<T> {
  typealias Element = T
  func convertFromArrayLiteral(_ elements: Element...) -> A {
    return A()
  }
}
