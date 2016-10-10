// RUN: %target-parse-verify-swift

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func throwing() throws -> Int? {}

func warnOptionalToAnyCoercion(value x: Int?) -> Any {
  let a: Any = x // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{17-17= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{17-17=!}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}{{17-17= as Any}}

  let b: Any = x as Any

  let c: Any = takeAny(a, b) // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}

  let _: Any = takeAny(c, c) as Any

  let _: Any = (x)  // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}

  let f: Any = (x as Any)
  let g: Any = (x) as (Any)

  _ = takeAny(f as? Int, g) // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}

  let _: Any = takeAny(f as? Int, g) as Any // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{33-33= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{33-33=!}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}{{33-33= as Any}}

  let _: Any = takeAny(f as? Int as Any, g) as Any

  let _: Any = x! == x! ? 1 : x // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}

  do {
    let _: Any = try throwing() // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
    // expected-note@-1 {{provide a default value to avoid this warning}}
    // expected-note@-2 {{force-unwrap the value to avoid this warning}}
    // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}
  } catch {}

  return x // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}
}

func warnOptionalInStringInterpolationSegment(_ o : Int?) {
  print("Always some, Always some, Always some: \(o)")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use '.debugDescription' to silence this warning}} {{52-52=.debugDescription}}
  // expected-note@-3 {{use 'as' to explicitly cast to 'Int?' to silence this warning}} {{52-52= as Int?}}
  print("Always some, Always some, Always some: \(o.map { $0 + 1 })")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use '.debugDescription' to silence this warning}} {{67-67=.debugDescription}}
  // expected-note@-3 {{use 'as' to explicitly cast to 'Int?' to silence this warning}} {{67-67= as Int?}}

  print("Always some, Always some, Always some: \(o as Int?)") // No warning
  print("Always some, Always some, Always some: \(o.debugDescription)") // No warning.
}

