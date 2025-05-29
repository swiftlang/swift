// RUN: %target-swift-frontend -typecheck %s -verify

// https://github.com/apple/swift/issues/55443

enum FooString: String { // expected-error {{'FooString' declares raw type 'String', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{add stubs for conformance}}
  case bar1 = #file // expected-error {{use of '#file' literal as raw value for enum case is not supported}}
  case bar2 = #function // expected-error {{use of '#function' literal as raw value for enum case is not supported}}
  case bar3 = #filePath // expected-error {{use of '#filePath' literal as raw value for enum case is not supported}}
  case bar4 = #line // expected-error {{cannot convert value of type 'Int' to raw type 'String'}}
  case bar5 = #column // expected-error {{cannot convert value of type 'Int' to raw type 'String'}}
  case bar6 = #dsohandle // expected-error {{cannot convert value of type 'UnsafeRawPointer' to raw type 'String'}}
}

enum FooInt: Int { // expected-error {{'FooInt' declares raw type 'Int', but does not conform to RawRepresentable and conformance could not be synthesized}} expected-note {{add stubs for conformance}}
  case bar1 = #file // expected-error {{cannot convert value of type 'String' to raw type 'Int'}}
  case bar2 = #function // expected-error {{cannot convert value of type 'String' to raw type 'Int'}}
  case bar3 = #filePath // expected-error {{cannot convert value of type 'String' to raw type 'Int'}}
  case bar4 = #line // expected-error {{use of '#line' literal as raw value for enum case is not supported}}
  case bar5 = #column // expected-error {{use of '#column' literal as raw value for enum case is not supported}}
  case bar6 = #dsohandle // expected-error {{cannot convert value of type 'UnsafeRawPointer' to raw type 'Int'}}
}
