// RUN: %target-swift-frontend -typecheck %s -verify

@propertyWrapper 
struct A {
  var wrappedValue: Int
}

@propertyWrapper 
struct B {
  var wrappedValue: Int
}

struct C {
  @A @B var foo: Int // expected-error{{composed wrapper type 'B' does not match former 'wrappedValue' type 'Int'}}
}
