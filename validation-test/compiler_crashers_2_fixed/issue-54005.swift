// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/54005

@propertyWrapper struct A {
  var wrappedValue: Int
}

@propertyWrapper struct B {
  var wrappedValue: Int
}

struct C {
  @A @B var foo: Int?
}
