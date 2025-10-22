// RUN: %target-swift-frontend -typecheck %s -verify

// https://github.com/apple/swift/issues/54004

@propertyWrapper 
struct A {
  var wrappedValue: Int
}

@propertyWrapper 
struct B {
  var wrappedValue: Int
}

struct C {
  @A @B var foo: Int // expected-error{{composed wrapper type 'B' does not match type of 'A.wrappedValue', which is 'Int'}}
}
