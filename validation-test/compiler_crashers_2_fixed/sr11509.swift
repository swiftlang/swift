// RUN: %target-swift-frontend %s -typecheck -verify

@propertyWrapper
public struct Wrapper<Value> {
  public init(someValue unused: Int) {
    _ = unused
  }
  
  public var wrappedValue: Value?
}


func functionScope() {
  let scopedValue = 3 // expected-note {{'scopedValue' declared here}}
  // expected-warning@-1 {{initialization of immutable value 'scopedValue' was never used; consider replacing with assignment to '_' or removing it}}
  
  enum StaticScope { // expected-note {{type declared here}}
    @Wrapper(someValue: scopedValue) static var foo: String? // expected-error {{enum declaration cannot close over value 'scopedValue' defined in outer scope}}
  }
  
  _ = StaticScope.foo
}
