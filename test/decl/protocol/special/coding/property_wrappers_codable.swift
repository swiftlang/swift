// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

@propertyWrapper
struct Wrapper<T: Codable> {
  var wrappedValue: T
}

@propertyWrapper
struct WrapperWithInitialValue<T: Codable> {
  var wrappedValue: T

  init(wrappedValue initialValue: T) {
    self.wrappedValue = initialValue
  }
}

extension WrapperWithInitialValue: Codable { }

struct X: Codable {
  @WrapperWithInitialValue var foo = 17

  // Make sure the generated key is named 'foo', like the original property.
  private func getFooKey() -> CodingKeys {
    return .foo
  }
}



// expected-error@+2{{type 'Y' does not conform to protocol 'Encodable'}}
// expected-error@+1{{type 'Y' does not conform to protocol 'Decodable'}}
struct Y: Codable {
  @Wrapper var foo: Int
  // expected-note@-1{{cannot automatically synthesize 'Encodable' because 'Wrapper<Int>' does not conform to 'Encodable'}}
  // expected-note@-2{{cannot automatically synthesize 'Decodable' because 'Wrapper<Int>' does not conform to 'Decodable'}}
  
}
