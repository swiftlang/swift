// RUN: %target-typecheck-verify-swift -swift-version 5

// https://github.com/apple/swift/issues/58661
class I58661 {
  func throwing<T>() throws -> T { // expected-note{{in call to function 'throwing()'}}
    throw Swift.fatalError()
  }

  func reproduce() {
    let check = try? throwing() // expected-error{{generic parameter 'T' could not be inferred}}
  }
}
