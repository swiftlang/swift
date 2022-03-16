// RUN: %target-typecheck-verify-swift

protocol P {
}

struct MyThing : P {
  var myVar: String? { "" }
}

struct Test {
  func test(thing: MyThing?) -> P {
    return thing?.myVar
    // expected-error@-1 {{return expression of type 'String' does not conform to 'P'}}
    // expected-error@-2 {{value of optional type 'String?' must be unwrapped to a value of type 'String'}}
    // expected-note@-3 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
    // expected-note@-4 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  }
}
