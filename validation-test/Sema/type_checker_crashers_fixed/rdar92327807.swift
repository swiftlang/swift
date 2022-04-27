// RUN: %target-typecheck-verify-swift

enum MyEnum {
case test
}

func test(result: MyEnum?) {
  if let .co = result { // expected-error {{pattern matching in a condition requires the 'case' keyword}}
    // expected-error@-1 {{type 'MyEnum?' has no member 'co'}}
  }
}
