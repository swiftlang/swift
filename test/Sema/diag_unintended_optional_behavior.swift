// RUN: %target-typecheck-verify-swift

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

func takesCollectionOfAny(_ a: [Any], _ d: [String : Any]) {
}

func warnCollectionOfAny(_ a: [Int?], _ d: [String : Int?]) {
  // https://bugs.swift.org/browse/SR-2928 - Collection casts from collections of optionals to collections of Any need custom handling
  takesCollectionOfAny(a, d) // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{25-25= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{25-25=!}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}{{25-25= as Any}}
  // expected-warning@-4 {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{28-28= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{28-28=!}}
  // expected-note@-7 {{explicitly cast to Any with 'as Any' to silence this warning}}{{28-28= as Any}}

  // https://bugs.swift.org/browse/SR-2928 - Collection casts from collections of optionals to collections of Any need custom handling
  takesCollectionOfAny(a as [Any], d as [String : Any]) // expected-warning {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{25-25= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{25-25=!}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence this warning}}{{25-25= as Any}}
  // expected-warning@-4 {{expression implicitly coerced from 'Int?' to Any}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{37-37= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{37-37=!}}
  // expected-note@-7 {{explicitly cast to Any with 'as Any' to silence this warning}}{{37-37= as Any}}
}

func warnOptionalInStringInterpolationSegment(_ o : Int?) {
  print("Always some, Always some, Always some: \(o)")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use 'String(describing:)' to silence this warning}} {{51-51=String(describing: }} {{52-52=)}} 
  // expected-note@-3 {{provide a default value to avoid this warning}} {{52-52= ?? <#default value#>}}
  var i: Int? = o
  print("Always some, Always some, Always some: \(i)")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use 'String(describing:)' to silence this warning}} {{51-51=String(describing: }} {{52-52=)}}
  // expected-note@-3 {{provide a default value to avoid this warning}} {{52-52= ?? <#default value#>}}
  i = nil
  print("Always some, Always some, Always some: \(o.map { $0 + 1 })")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use 'String(describing:)' to silence this warning}} {{51-51=String(describing: }} {{67-67=)}} 
  // expected-note@-3 {{provide a default value to avoid this warning}} {{67-67= ?? <#default value#>}}

  print("Always some, Always some, Always some: \(o as Int?)") // No warning
  print("Always some, Always some, Always some: \(o.debugDescription)") // No warning.
}
