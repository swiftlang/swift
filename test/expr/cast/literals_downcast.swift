// RUN: %target-typecheck-verify-swift

let ok = "A" as Character // OK
let succeed = "A" as? String // expected-warning {{always succeeds}}
let force = "A" as! String // expected-warning {{forced cast of 'String' to same type has no effect}}
let bad = "A" as? Character // expected-warning {{cast from literal of inferred type 'String' to unrelated type 'Character' always fails; consider using 'as' coercion}} {{none}}
let bad2 = "Aa" as? Character // expected-warning {{cast from 'String' to unrelated type 'Character' always fails}}
let badForce = "Aa" as! Character // expected-warning {{cast from 'String' to unrelated type 'Character' always fails}}
let bad1 = 1 as? Character // expected-warning {{cast from 'Int' to unrelated type 'Character' always fails}}
let bad1Force = 1 as! Character // expected-warning {{cast from 'Int' to unrelated type 'Character' always fails}}

let okInt = 1 as Int // OK
let IntForce = 1 as! Int // expected-warning {{forced cast of 'Int' to same type has no effect}}
let badInt = 1 as? Int // expected-warning {{always succeeds}}
let badInt1 = 1.0 as? Int // expected-warning {{cast from 'Double' to unrelated type 'Int' always fails}}
let badInt2 = 1 as? Double // expected-warning {{cast from literal of inferred type 'Int' to unrelated type 'Double' always fails; consider using 'as' coercion}} {{none}}
let badInt3 = 1 as! Double // expected-warning {{cast from literal of inferred type 'Int' to unrelated type 'Double' always fails; consider using 'as' coercion}}
let badInt4 = 1.0 as! Int // expected-warning {{cast from 'Double' to unrelated type 'Int' always fails}}

let okUInt = 1 as UInt // OK
let badUInt = 1 as? UInt // expected-warning {{cast from literal of inferred type 'Int' to unrelated type 'UInt' always fails; consider using 'as' coercion}} {{none}}
let badUInt1 = 1.0 as? UInt // expected-warning {{cast from 'Double' to unrelated type 'UInt' always fails}}
let badUInt2 = 1.0 as! UInt // expected-warning {{cast from 'Double' to unrelated type 'UInt' always fails}}
let badUInt3 = 1 as! UInt // expected-warning {{cast from literal of inferred type 'Int' to unrelated type 'UInt' always fails; consider using 'as' coercion}}

// Custom protocol adoption
struct S: ExpressibleByStringLiteral {
  typealias StringLiteralType = String
  init(stringLiteral value: Self.StringLiteralType) {}
}

let a = "A" as? S // expected-warning {{cast from literal of inferred type 'String' to unrelated type 'S' always fails; consider using 'as' coercion}} {{none}}
let a1 = "A" as! S // expected-warning {{cast from literal of inferred type 'String' to unrelated type 'S' always fails; consider using 'as' coercion}}
