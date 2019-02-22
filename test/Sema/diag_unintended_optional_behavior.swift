// RUN: %target-typecheck-verify-swift

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func throwing() throws -> Int? {}

func warnOptionalToAnyCoercion(value x: Int?) -> Any {
  let a: Any = x // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{17-17= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{17-17=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{17-17= as Any}}

  let b: Any = x as Any

  let c: Any = takeAny(a, b) // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}

  let _: Any = takeAny(c, c) as Any

  let _: Any = (x)  // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}

  let f: Any = (x as Any)
  let g: Any = (x) as (Any)

  _ = takeAny(f as? Int, g) // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}

  let _: Any = takeAny(f as? Int, g) as Any // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{33-33= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{33-33=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{33-33= as Any}}

  let _: Any = takeAny(f as? Int as Any, g) as Any

  let _: Any = x! == x! ? 1 : x // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}

  do {
    let _: Any = try throwing() // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
    // expected-note@-1 {{provide a default value to avoid this warning}}
    // expected-note@-2 {{force-unwrap the value to avoid this warning}}
    // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}
  } catch {}

  return x // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}
}

func takesOptionalAny(_: Any?, _: Any?) {}

func warnNestedOptionalToOptionalAnyCoercion(_ a: Int?, _ b: Any??, _ c: Int???, _ d: Any????) {
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

// No warnings in Swift 3/4 for IUO-to-Any coercion.
func nowarnIUOToAnyCoercion(_ a: Int!, _ b: Any?!) {
  _ = takeAny(a, b)
  _ = takeAny(returningIUO(), C().returningIUO())
  _ = takeAny(C().a, C().b)
  _ = takeAny(C()[0], C()[1.0])
  _ = takeAny(D(), D())

  _ = takeAny(a as Any, b as Any)
}

// No warnings in Swift 3/4 for IUO-to-Any coercion.
func nowarnIUOToOptionalAnyCoercion(_ a: Int!, _ b: Any?!, _ c: Int??!, _ d: Any???!) {
  takesOptionalAny(a, b)

  takesOptionalAny(a, b ?? "")
  takesOptionalAny(a, b!)
  takesOptionalAny(a, b as Any?)

  takesOptionalAny(c, d)

  takesOptionalAny(c!!, d!!!)
  takesOptionalAny(c as Any?, d as Any?)
}

func takesIUO(_: Any!, _: Any!) {}

func warnOptionalToIUOAny(_ a: Int?, _ b: Any??, _ c: Int???, _ d: Any????) {
  takesIUO(a, b) // expected-warning {{expression implicitly coerced from 'Any??' to 'Any?'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{16-16= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{16-16=!}}
  // expected-note@-3 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{16-16= as Any?}}

  takesIUO(a, b ?? "")
  takesIUO(a, b!)
  takesIUO(a, b as Any?)

  takesIUO(c, d) // expected-warning {{expression implicitly coerced from 'Int???' to 'Any?'}}
  // expected-note@-1 {{force-unwrap the value to avoid this warning}}{{13-13=!!}}
  // expected-note@-2 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{13-13= as Any?}}
  // expected-warning@-3 {{expression implicitly coerced from 'Any????' to 'Any?'}}
  // expected-note@-4 {{force-unwrap the value to avoid this warning}}{{16-16=!!!}}
  // expected-note@-5 {{explicitly cast to 'Any?' with 'as Any?' to silence this warning}}{{16-16= as Any?}}

  takesIUO(c!!, d!!!)
  takesIUO(c as Any?, d as Any?)
}

func takesCollectionOfAny(_ a: [Any], _ d: [String : Any]) {}
func takesCollectionOfOptionalAny(_ a: [Any?], _ d: [String : Any?]) {}

func warnCollectionOfOptionalToAnyCoercion(_ a: [Int?], _ d: [String : Int?]) {
  takesCollectionOfAny(a, d) // expected-warning {{expression implicitly coerced from '[Int?]' to '[Any]'}}
  // expected-note@-1 {{explicitly cast to '[Any]' with 'as [Any]' to silence this warning}}{{25-25= as [Any]}}
  // expected-warning@-2 {{expression implicitly coerced from '[String : Int?]' to '[String : Any]'}}
  // expected-note@-3 {{explicitly cast to '[String : Any]' with 'as [String : Any]' to silence this warning}}{{28-28= as [String : Any]}}
  takesCollectionOfAny([a[0]], ["test" : a[0]]) // expected-warning {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-1 {{provide a default value to avoid this warning}}{{29-29= ?? <#default value#>}}
  // expected-note@-2 {{force-unwrap the value to avoid this warning}}{{29-29=!}}
  // expected-note@-3 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{29-29= as Any}}
  // expected-warning@-4 {{expression implicitly coerced from 'Int?' to 'Any'}}
  // expected-note@-5 {{provide a default value to avoid this warning}}{{46-46= ?? <#default value#>}}
  // expected-note@-6 {{force-unwrap the value to avoid this warning}}{{46-46=!}}
  // expected-note@-7 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}{{46-46= as Any}}

  takesCollectionOfAny(a as [Any], d as [String : Any])
}

func nowarnCollectionOfIUOToAnyCoercion(_ a: Int!) {
  takesCollectionOfAny([a], ["test" : a])
}

func warnCollectionOfTripleOptionalToAnyCoercion(_ a: [Any???], _ d: [String: Any???]) {
  takesCollectionOfAny(a, d) // expected-warning {{expression implicitly coerced from '[Any???]' to '[Any]'}}
  // expected-note@-1 {{explicitly cast to '[Any]' with 'as [Any]' to silence this warning}}{{25-25= as [Any]}}
  // expected-warning@-2 {{expression implicitly coerced from '[String : Any???]' to '[String : Any]'}}
  // expected-note@-3 {{explicitly cast to '[String : Any]' with 'as [String : Any]' to silence this warning}}{{28-28= as [String : Any]}}

  takesCollectionOfAny(a as [Any], d as [String : Any])

  takesCollectionOfOptionalAny(a, d) // expected-warning {{expression implicitly coerced from '[Any???]' to '[Any?]'}}
  // expected-note@-1 {{explicitly cast to '[Any?]' with 'as [Any?]' to silence this warning}}{{33-33= as [Any?]}}
  // expected-warning@-2 {{expression implicitly coerced from '[String : Any???]' to '[String : Any?]'}}
  // expected-note@-3 {{explicitly cast to '[String : Any?]' with 'as [String : Any?]' to silence this warning}}{{36-36= as [String : Any?]}}

  takesCollectionOfOptionalAny(a as [Any?], d as [String : Any?])
}

struct SpecialType {}

extension DefaultStringInterpolation {
  // An interpolator which explicitly contemplates that its value might be
  // optional.
  mutating func appendInterpolation(_ expr: SpecialType?) {
    appendInterpolation(expr as Any)
  }
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
  
  let oST = Optional(SpecialType())
  let ooST = Optional(oST)
  print("Always some, Always some, Always some: \(oST)") // No warning.
  
  print("Always some, Always some, Always some: \(ooST)")
  // expected-warning@-1 {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-2 {{use 'String(describing:)' to silence this warning}} {{51-51=String(describing: }} {{55-55=)}} 
  // expected-note@-3 {{provide a default value to avoid this warning}} {{55-55= ?? <#default value#>}}  
}
