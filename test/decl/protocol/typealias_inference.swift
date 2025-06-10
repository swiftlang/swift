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

struct Base<Value>: BaseProtocol { // expected-error {{circular reference}}
// expected-note@-1 {{through reference here}}
  private let closure: Closure

  init(closure: Closure) {
  // expected-note@-1 {{while resolving type 'Closure'}}
  // expected-note@-2 2{{through reference here}}
    self.closure = closure
  }
}
