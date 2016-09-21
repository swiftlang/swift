// RUN: %target-parse-verify-swift

func takeAny(_ left: Any, _ right: Any) -> Int? {
  return left as? Int
}

func throwing() throws -> Int? {}

func warnOptionalToAnyCoercion(value x: Int?) -> Any {
  let a: Any = x // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  let b: Any = x as Any

  let c: Any = takeAny(a, b) // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  let d: Any = takeAny(c, c) as Any

  let e: Any = (x)  // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  _ = takeAny(d, e)

  let f: Any = (x as Any)
  let g: Any = (x) as (Any)

  _ = takeAny(f as? Int, g) // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  let h: Any = takeAny(f as? Int, g) as Any // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  let i: Any = takeAny(f as? Int as Any, g) as Any

  _ = takeAny(h, i)

  let j: Any = x! == x! ? 1 : x // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}

  let k: Any
  do {
    k = try throwing() // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
    // expected-note@-1 {{provide a default value to avoid the warning}}
    // expected-note@-2 {{force-unwrap the value to avoid the warning}}
    // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}
  } catch {}

  _ = takeAny(j, k)

  return x // expected-warning {{expression implicitly coerced from 'Optional<Int>' to Any}}
  // expected-note@-1 {{provide a default value to avoid the warning}}
  // expected-note@-2 {{force-unwrap the value to avoid the warning}}
  // expected-note@-3 {{explicitly cast to Any with 'as Any' to silence}}
}
