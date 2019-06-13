// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/keypath.swift -primary-file %s

struct S {
  let i: Int

  init() {
    let _: WritableKeyPath<S, Int> = \.i // no error for Swift 3/4

    S()[keyPath: \.i] = 1
    // expected-error@-1 {{cannot assign through subscript: function call returns immutable value}}
  }
}

func test() {
  let _: WritableKeyPath<C, Int> = \.i // no error for Swift 3/4

  C()[keyPath: \.i] = 1   // warning on write with literal keypath
  // expected-warning@-1 {{forming a writable keypath to property}}

  let _ = C()[keyPath: \.i] // no warning for a read
}

// SR-7339
class Some<T, V> { // expected-note {{'V' declared as parameter to type 'Some'}}
  init(keyPath: KeyPath<T, ((V) -> Void)?>) {
  }
}

class Demo {
  var here: (() -> Void)?
}

// FIXME: This error is better than it was, but the diagnosis should break it down more specifically to 'here's type.
// TODO(diagnostics): Fix this regression, we want the following error message ideally
// "cannot convert value of type 'ReferenceWritableKeyPath<Demo, (() -> Void)?>' to expected argument type 'KeyPath<_, ((_) -> Void)?>'"
let some = Some(keyPath: \Demo.here)
// expected-error@-1 {{generic parameter 'V' could not be inferred}}
// expected-note@-2 {{explicitly specify the generic arguments to fix this issue}}

