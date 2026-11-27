// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/94567
// A string literal that is a single extended grapheme cluster according to
// the Indic conjunct rule (UAX #29, GB9c) must be usable as a `Character`,
// just as it is at run time.

// Consonant + virama + consonant.
let _: Character = "\u{915}\u{94D}\u{937}"
let _: Character = "क्ष"

// Longer conjunct.
let _: Character = "\u{938}\u{94D}\u{924}\u{94D}\u{930}"
let _: Character = "स्त्र"

// Other scripts.
let _: Character = "\u{995}\u{9CD}\u{9B7}" // Bengali
let _: Character = "\u{D15}\u{D4D}\u{D15}" // Malayalam

// An Extend scalar or ZWJ may surround the linker.
let _: Character = "\u{915}\u{93C}\u{94D}\u{937}"
let _: Character = "\u{915}\u{94D}\u{200D}\u{937}"

// Still an inferred `Character`, not a `String`.
let c = "\u{915}\u{94D}\u{937}" as Character
_ = c

// These are not conjuncts, so they are more than one grapheme cluster.
let _: Character = "\u{915}\u{94D}a" // expected-error {{cannot convert value of type 'String' to specified type 'Character'}}
let _: Character = "\u{915}\u{937}" // expected-error {{cannot convert value of type 'String' to specified type 'Character'}}
let _: Character = "\u{915}\u{94D}\u{200C}\u{937}" // expected-error {{cannot convert value of type 'String' to specified type 'Character'}}
let _: Character = "\u{94D}\u{937}" // expected-error {{cannot convert value of type 'String' to specified type 'Character'}}
