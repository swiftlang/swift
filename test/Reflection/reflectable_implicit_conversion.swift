// RUN: %target-swift-frontend -typecheck %s -verify

func cast<T, U>(_ x: U) -> T {
  return x as! T
}

func castOptional<T, U>(_ x: U) -> T? {
  return x as? T
}

public func wrap(
  _ testFunction: @escaping () -> Void
) { testFunction() }

public func consume(_ t: Reflectable) {}

let a: Reflectable? = cast(1) // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}
let a1: Reflectable = cast(1) // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}

let b: Reflectable? = castOptional(1) // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}
let b1: Reflectable = castOptional(1)! // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}

let c = cast(1) as Reflectable // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}
let c1 = cast(1) as Reflectable? // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}

let d = castOptional(1) as Reflectable? // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}
let d1 = castOptional(1)! as Reflectable // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}

wrap() {
   let e: Reflectable? = castOptional(1) // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}
}

consume(cast(1)) // expected-error {{expression can't be implicitly converted to Reflectable; use 'as? Reflectable' or 'as! Reflectable' instead}}