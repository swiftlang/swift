class A<T> {
  typealias Element = T
  func convertFromArrayLiteral(elements: Element...) -> A {
    return A()
  }
}
