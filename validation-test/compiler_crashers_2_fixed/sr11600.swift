// RUN: not %target-swift-frontend -typecheck %s

@propertyWrapper struct A {
  var wrappedValue: Int
}

@propertyWrapper struct B {
  var wrappedValue: Int
}

struct C {
  @A @B var foo: Int?
}
