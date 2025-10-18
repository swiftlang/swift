// RUN: %target-swift-frontend %s -typecheck -verify

// https://github.com/apple/swift/issues/54093

@propertyWrapper 
struct Wrapper1 {
  var wrappedValue: Int?
}

class Test1 {
  @Wrapper1 var user: Int 
  // expected-error@-1 {{property type 'Int' does not match 'wrappedValue' type 'Int?'}}
}

@propertyWrapper 
struct Wrapper2 {
  var wrappedValue: Int??
}

class Test2 {
  @Wrapper2 var user: Int? 
  // expected-error@-1 {{property type 'Int?' does not match 'wrappedValue' type 'Int??'}}
  // expected-note@-2 {{arguments to generic parameter 'Wrapped' ('Int' and 'Int?') are expected to be equal}}
}
