// RUN: %target-typecheck-verify-swift -swift-version 5

// Additional warnings produced in Swift 5+ mode.

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func takesOptionalAny(_: Any?, _: Any?) {}

class C {
  var a: Int!
  var b: Any?!
  func returningIUO() -> Int! { return a }
  func returningAny() -> Any { return a }

  subscript(i: Int) -> Int! { return 0 }
  subscript(i: Float) -> Any! { return 0 }
}

class D {
  init!() {}
}

func returningIUO() -> Int! { return 1 }

func warnIUOToAnyCoercion(_ a: Int!, _ b: Any?!) {
  _ = takeAny(a, b) // expected-warning {{expression implicitly coerced from 'Any??' to 'Any'}}
  // expected-note@-1 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{19-19= as Any}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{19-19=!!}}
  _ = takeAny(returningIUO(), C().returningIUO())
  _ = takeAny(C().a, C().b) // expected-warning {{expression implicitly coerced from 'Any??' to 'Any'}}
  // expected-note@-1 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{27-27= as Any}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{27-27=!!}}
  _ = takeAny(C()[0], C()[1.0])
  _ = takeAny(D(), D())

  _ = takeAny(a as Any, b as Any)
}

func warnIUOToOptionalAnyCoercion(_ a: Int!, _ b: Any?!, _ c: Int??!, _ d: Any???!) {
  takesOptionalAny(a, b) // expected-warning {{expression implicitly coerced from 'Any??' to 'Any?'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{24-24= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{24-24=!}}
  // expected-note@-3 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{24-24= as Any?}}

  takesOptionalAny(a, b ?? "")
  takesOptionalAny(a, b!)
  takesOptionalAny(a, b as Any?)

  takesOptionalAny(c, d) // expected-warning {{expression implicitly coerced from 'Int???' to 'Any?'}}
  // expected-note@-1 {{force-unwrap the value to avoid this warning}}{{21-21=!!}}
  // expected-note@-2 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{21-21= as Any?}}
  // expected-warning@-3 {{expression implicitly coerced from 'Any????' to 'Any?'}}
  // expected-note@-4 {{force-unwrap the value to avoid this warning}}{{24-24=!!!}}
  // expected-note@-5 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{24-24= as Any?}}

  takesOptionalAny(c!!, d!!!)
  takesOptionalAny(c as Any?, d as Any?)
}

func takesCollectionOfAny(_ a: [Any], _ d: [String : Any]) {}

func warnCollectionOfIUOToAnyCoercion(_ a: Int!) {
  takesCollectionOfAny([a], ["test" : a])
}
