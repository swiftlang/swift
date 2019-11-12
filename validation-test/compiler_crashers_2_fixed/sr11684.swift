// RUN: %target-swift-frontend %s -typecheck -verify

@propertyWrapper 
struct Wrapper1 { // expected-note {{property wrapper type 'Wrapper1' declared here}}
  var wrappedValue: Int?
}

class Test1 {
  @Wrapper1 var user: Int 
  // expected-error@-1 {{property type 'Int' does not match that of the 'wrappedValue' property of its wrapper type 'Wrapper1'}}
}

@propertyWrapper 
struct Wrapper2 { // expected-note {{property wrapper type 'Wrapper2' declared here}}
  var wrappedValue: Int??
}

class Test2 {
  @Wrapper2 var user: Int? 
  // expected-error@-1 {{property type 'Int?' does not match that of the 'wrappedValue' property of its wrapper type 'Wrapper2'}}
}
