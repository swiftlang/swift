// RUN: %target-swift-frontend -typecheck %s -verify

// https://github.com/apple/swift/issues/55443

enum FooString: String {
  case bar1 = #file // expected-error {{raw value for enum case must be a literal}}
  case bar2 = #function // expected-error {{raw value for enum case must be a literal}}
  case bar3 = #filePath // expected-error {{raw value for enum case must be a literal}}
  case bar4 = #line // expected-error {{raw value for enum case must be a literal}}
  case bar5 = #column // expected-error {{raw value for enum case must be a literal}}
  case bar6 = #dsohandle // expected-error {{raw value for enum case must be a literal}}
}

enum FooInt: Int {
  case bar1 = #file // expected-error {{raw value for enum case must be a literal}}
  case bar2 = #function // expected-error {{raw value for enum case must be a literal}}
  case bar3 = #filePath // expected-error {{raw value for enum case must be a literal}}
  case bar4 = #line // expected-error {{raw value for enum case must be a literal}}
  case bar5 = #column // expected-error {{raw value for enum case must be a literal}}
  case bar6 = #dsohandle // expected-error {{raw value for enum case must be a literal}}
}
