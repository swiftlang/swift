// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/51321
//
// By itself in this file because this particular expected error is omitted if
// there have been any other diagnostics.

protocol BaseProtocol {
  associatedtype Value
  typealias Closure = () -> Value

  init(closure: Closure)
}

struct Base<Value>: BaseProtocol {
  private let closure: Closure

  init(closure: Closure) { //expected-error {{reference to invalid type alias 'Closure' of type 'Base<Value>'}}
    self.closure = closure
  }
}
