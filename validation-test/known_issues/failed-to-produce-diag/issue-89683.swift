// rdar://178724307 (https://github.com/swiftlang/swift/issues/89683)
// RUN: %target-typecheck-verify-swift

class A<Element>: B<Element> { // expected-error {{'B' requires that 'Element' be a class type}}
  func a() {
    let _ = b // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}

class B<Element: AnyObject> { // expected-note {{requirement specified as 'Element' : 'AnyObject' [with Element = Element]}}
  var b = 0
}
