// RUN: %target-typecheck-verify-swift

let ok = "A" as Character // OK
let succeed = "A" as? String // expected-warning {{always succeeds}}
let bad = "A" as? Character // expected-warning {{conditional downcast from literal to 'Character' always fails; consider using 'as' coercion}} {{none}}
let bad2 = "Aa" as? Character // expected-warning {{cast from 'String' to unrelated type 'Character' always fails}}
let bad1 = 1 as? Character // expected-warning {{cast from 'Int' to unrelated type 'Character' always fails}}

let okInt = 1 as Int // OK
let badInt = 1 as? Int // expected-warning {{always succeeds}}
let badInt1 = 1.0 as? Int // expected-warning {{cast from 'Double' to unrelated type 'Int' always fails}}
let badInt2 = 1 as? Double // expected-warning {{conditional downcast from literal to 'Double' always fails; consider using 'as' coercion}} {{none}}

let okUInt = 1 as UInt // OK
let badUInt = 1 as? UInt // expected-warning {{conditional downcast from literal to 'UInt' always fails; consider using 'as' coercion}} {{none}}
let badUInt1 = 1.0 as? UInt // expected-warning {{cast from 'Double' to unrelated type 'UInt' always fails}}

// Custom protocol adoption
struct S: ExpressibleByStringLiteral {
  typealias StringLiteralType = String
  init(stringLiteral value: Self.StringLiteralType) {}
}

let a = "A" as? S // expected-warning {{conditional downcast from literal to 'S' always fails; consider using 'as' coercion}} {{none}}
