// RUN: %target-typecheck-verify-swift -swift-version 5

// Additional warnings produced in Swift 5+ mode.

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func takesOptionalAny(_: Any?, _: Any?) {}

class C {
  var a: Int! // expected-note 2{{implicitly unwrapped property 'a' declared here}}
  var b: Any?! // expected-note {{implicitly unwrapped property 'b' declared here}}
  func returningIUO() -> Int! { return a } // expected-note {{instance method 'returningIUO()' with implicitly unwrapped result type is declared here}}
  func returningAny() -> Any { return a } // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{40-40= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{40-40=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{40-40= as Any}}

  subscript(i: Int) -> Int! { return 0 } // expected-note {{implicitly unwrapped subscript 'subscript(_:)' declared here}}
  subscript(i: Float) -> Any! { return 0 } // expected-note {{implicitly unwrapped subscript 'subscript(_:)' declared here}}
}

class D {
  init!() {} // expected-note 2{{implicitly unwrapped initializer 'init()' declared here}}
}

func returningIUO() -> Int! { return 1 } // expected-note {{global function 'returningIUO()' with implicitly unwrapped result type is declared here}}

func warnIUOToAnyCoercion(_ a: Int!, _ b: Any?!) { // expected-note {{implicitly unwrapped parameter 'a' declared here}} // expected-note {{implicitly unwrapped parameter 'b' declared here}}
  _ = takeAny(a, b) // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{16-16= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{16-16=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{16-16= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'Any??' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{force-unwrap the value to avoid this warning}}{{19-19=!!}}
  // expected-note@-6 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{19-19= as Any}}
  _ = takeAny(returningIUO(), C().returningIUO()) // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{29-29= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{29-29=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{29-29= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{49-49= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{49-49=!}}
  // expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{49-49= as Any}}
  _ = takeAny(C().a, C().b) // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{20-20= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{20-20=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{20-20= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'Any??' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{force-unwrap the value to avoid this warning}}{{27-27=!!}}
  // expected-note@-6 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{27-27= as Any}}
  _ = takeAny(C()[0], C()[1.0]) // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{21-21= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{21-21=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{21-21= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'Any?' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{31-31= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{31-31=!}}
  // expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{31-31= as Any}}
  _ = takeAny(D(), D()) // expected-warning {{coercion of implicitly unwrappable value of type 'D?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{18-18= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{18-18=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{18-18= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'D?' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{23-23= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{23-23=!}}
  // expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{23-23= as Any}}

  _ = takeAny(a as Any, b as Any)
}

func warnIUOToOptionalAnyCoercion(_ a: Int!, _ b: Any?!, _ c: Int??!, _ d: Any???!) { // expected-note {{implicitly unwrapped parameter 'b' declared here}} // expected-note {{implicitly unwrapped parameter 'c' declared here}} // expected-note {{implicitly unwrapped parameter 'd' declared here}}
  takesOptionalAny(a, b) // expected-warning {{coercion of implicitly unwrappable value of type 'Any??' to 'Any?' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{24-24= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{24-24=!}}
  // expected-note@-3 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{24-24= as Any?}}

  takesOptionalAny(a, b ?? "")
  takesOptionalAny(a, b!)
  takesOptionalAny(a, b as Any?)

  takesOptionalAny(c, d) // expected-warning {{coercion of implicitly unwrappable value of type 'Int???' to 'Any?' does not unwrap optional}}
  // expected-note@-1 {{force-unwrap the value to avoid this warning}}{{21-21=!!}}
  // expected-note@-2 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{21-21= as Any?}}
  // expected-warning@-3 {{coercion of implicitly unwrappable value of type 'Any????' to 'Any?' does not unwrap optional}}
  // expected-note@-4 {{force-unwrap the value to avoid this warning}}{{24-24=!!!}}
  // expected-note@-5 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{24-24= as Any?}}

  takesOptionalAny(c!!, d!!!)
  takesOptionalAny(c as Any?, d as Any?)
}

func takesCollectionOfAny(_ a: [Any], _ d: [String : Any]) {}

func warnCollectionOfIUOToAnyCoercion(_ a: Int!) { // expected-note 2{{implicitly unwrapped parameter 'a' declared here}}
  takesCollectionOfAny([a], ["test" : a]) // expected-warning {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{26-26= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{26-26=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{26-26= as Any}}
  // expected-warning@-4 {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{40-40= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{40-40=!}}
  // expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{40-40= as Any}}
}

func takesAny_sr10199(_ x: Any) {}

let fn_sr10199: (() -> Int?)! = { return nil }
takesAny_sr10199(fn_sr10199()) // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
// expected-note@-1 {{provide a default value to avoid this warning}}
// expected-note@-2 {{force-unwrap the value to avoid this warning}}
// expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}
