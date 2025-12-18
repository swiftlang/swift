// RUN: not %target-swift-frontend -typecheck %s

open class Foo {
  open class func foo<T: Equatable>(value: T?, other: T?) {
    switch (value, other) {
    case (.some(let unwrappedValue &), .none): // the & here triggers it
      fallthrough
    default:
      fatalError()
    }
  }
}
