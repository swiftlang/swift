public protocol HasDefaultFoo {}
extension HasDefaultFoo {
  internal func foo() {}
}

internal class Base {
  func foo() {}
}
