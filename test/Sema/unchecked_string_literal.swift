// RUN: %target-typecheck-verify-swift -disable-availability-checking

// A string literal containing a `\x{hh}` raw code unit escape defaults to
// `UncheckedString<UInt8>`, never to `String`, so a typo like `\x{2041}`
// (rather than `\x{20}\x{41}`) is caught immediately at the literal.
let defaultsToUInt8 = "Ren\x{e9} Descartes"
let alsoDefaultsToUInt8: UncheckedString<UInt8> = defaultsToUInt8

// `String` never accepts a `\x{hh}` escape.
let invalidAsString: String = "Ren\x{e9} Descartes" // expected-error {{cannot convert value of type 'UncheckedString<UInt8>' to specified type 'String'}}

// Widening to a wider `Element` via contextual type is fine, provided the
// raw escape's value fits.
let explicitUInt16: UncheckedString<UInt16> = "Ren\x{2041} Descartes"
let explicitUInt32: UncheckedString<UInt32> = "Ren\x{11003} Descartes"

// `\u{hh}` never triggers the raw-escape overflow diagnostic, regardless of
// `Element` width, since it denotes a Unicode scalar (transcoded to the
// target width), not a raw code unit.
let unicodeEscapeUInt8: UncheckedString<UInt8> = "Ren\u{e9} Descartes"
let unicodeEscapeWide: UncheckedString<UInt16> = "\u{11003}"

// A `\x{hh}` value that doesn't fit `Element`'s width is diagnosed at the
// escape itself.
let overflowsUInt8: UncheckedString<UInt8> = "\x{100}" // expected-error {{raw code unit escape does not fit in 'UInt8' (8-bit)}}
let overflowsDefaultUInt8 = "\x{100}" // expected-error {{raw code unit escape does not fit in 'UInt8' (8-bit)}}
let overflowsUInt16: UncheckedString<UInt16> = "\x{10000}" // expected-error {{raw code unit escape does not fit in 'UInt16' (16-bit)}}

// Multiple offending escapes in one literal are each diagnosed.
let multipleOverflows: UncheckedString<UInt8> = "\x{100}\x{200}"
// expected-error@-1 {{raw code unit escape does not fit in 'UInt8' (8-bit)}}
// expected-error@-2 {{raw code unit escape does not fit in 'UInt8' (8-bit)}}

// A `\x{hh}` value that fits exactly at the boundary is fine.
let fitsExactlyUInt8: UncheckedString<UInt8> = "\x{ff}"
let fitsExactlyUInt16: UncheckedString<UInt16> = "\x{ffff}"

// A literal with no `\x{hh}`/`\u{hh}` escape at all, contextually widened to
// a non-`UInt8` `UncheckedString`, still routes through
// `ExpressibleByUncheckedStringLiteral` (not `ExpressibleByStringLiteral`),
// so the compiler materializes a native-width constant rather than a UTF-8
// constant that would need transcoding at runtime.
let plainTextWidened: UncheckedString<UInt16> = "no escapes here"

// `String` itself is unaffected by any of the above -- ordinary string
// literals still resolve to `ExpressibleByStringLiteral` as before.
let ordinaryString: String = "no escapes here"

// MARK: `as`-coercion and call-syntax routing
//
// A splice-free string literal conforms to the `ExpressibleByPossiblyUncheckedStringLiteral`
// umbrella (not directly to `ExpressibleByStringLiteral`), so ordinary
// constraint solving can resolve it to `UncheckedString<Element>` under
// *any* kind of context -- a declared type, an `as`-coercion, or call-syntax
// literal-init sugar -- without needing special-case handling for each.

// `as`-coercion, no escapes at all.
let asCoercionPlain = "no escapes here" as UncheckedString<UInt16>

// `as`-coercion with a `\x{hh}` escape that fits.
let asCoercionRaw = "Ren\x{2041} Descartes" as UncheckedString<UInt16>

// `as`-coercion with a `\x{hh}` escape that overflows -- the overflow
// diagnostic must still fire through this path.
let asCoercionOverflow = "\x{10000}" as UncheckedString<UInt16>
// expected-error@-1 {{raw code unit escape does not fit in 'UInt16' (16-bit)}}

// Call-syntax, no escapes at all.
let callSyntaxPlain = UncheckedString<UInt16>("no escapes here")

// Call-syntax with a `\x{hh}` escape that fits.
let callSyntaxRaw = UncheckedString<UInt16>("Ren\x{2041} Descartes")

// Call-syntax with a `\x{hh}` escape that overflows.
let callSyntaxOverflow = UncheckedString<UInt16>("\x{10000}")
// expected-error@-1 {{raw code unit escape does not fit in 'UInt16' (16-bit)}}

// `String` itself is unaffected: `as String`/`String(...)` on an ordinary
// literal still works, and still rejects a `\x{hh}` escape.
let stringAsCoercion = "plain text" as String
let stringCallSyntax = String("plain text")
let stringAsCoercionInvalid = "Ren\x{e9} Descartes" as String
// expected-error@-1 {{cannot convert value of type 'UncheckedString<UInt8>' to type 'String' in coercion}}

// MARK: Local (closure/function-body) pattern bindings
//
// A local `let`/`var` binding's initializer -- checked as part of a
// closure's or function's *joint* constraint system, rather than as its
// own independent top-level expression -- goes through a different code
// path (`CSSyntacticElement.cpp`'s `visitPatternBindingElement`) that
// doesn't register the binding's contextual type before generating
// constraints for its initializer the way top-level pattern bindings do.
// Regression test: this must route through
// `ExpressibleByUncheckedStringLiteral` exactly like a top-level binding.
func localBindingInFunctionBody() {
  let s: UncheckedString<UInt16> = "Ren\x{2041} Descartes"
  _ = s
}

let localBindingInClosure: () -> Void = {
  let s: UncheckedString<UInt16> = "Ren\x{2041} Descartes"
  _ = s
}

// The overflow diagnostic must still fire for a local binding too.
func localBindingOverflowInFunctionBody() {
  let s: UncheckedString<UInt16> = "\x{10000}"
  // expected-error@-1 {{raw code unit escape does not fit in 'UInt16' (16-bit)}}
  _ = s
}

// A closure passed as a default parameter value is a separate contextual-
// type-registration path (`CTP_DefaultParameter`) from a local pattern
// binding's initializer (`CTP_Initialization`); make sure fixing the
// latter didn't regress the former by double-registering contextual info
// for the whole closure. (This isn't about `UncheckedString` routing at
// all -- it's a plain closure body containing a local binding of any
// type -- but it's exactly the shape that previously crashed the
// compiler with an internal assertion failure.)
func defaultParameterClosure(_ body: () -> Void = {
  let x = 5
  _ = x
}) {
  body()
}

// MARK: Umbrella-protocol redesign: operators, single-character literals,
// and interpolation

// A concrete, non-generic `+` overload lets literal operands (including
// single-character ones, and splice-containing ones) resolve under context,
// unlike `RangeReplaceableCollection`'s fully generic `+`, whose `Other`
// parameter names no concrete type for the solver to try.
let plusMultiChar: UncheckedString<UInt8> = "ab" + "cd"
let plusSingleChar: UncheckedString<UInt8> = "ab" + "c"
let plusWithSplice: UncheckedString<UInt8> = "ab" + "\x{41}"

// A single-character literal now conforms to the "possibly unchecked"
// unicode-scalar/grapheme-cluster umbrellas (mirroring plain literals'
// `ExpressibleByPossiblyUncheckedStringLiteral`), so it can resolve to
// `UncheckedString<Element>` under context instead of being locked
// unconditionally to `Character`/`Unicode.Scalar`/`String`.
let singleCharDirect: UncheckedString<UInt8> = "c"
let singleCharCoercion = "c" as UncheckedString<UInt8>
let singleCharCallSyntax = UncheckedString<UInt8>("c")

// Ordinary `Character`/`Unicode.Scalar`/`String` single-character literals
// must be completely unaffected by the above.
let ordinaryCharacter: Character = "x"
let ordinaryScalar: Unicode.Scalar = "y"
let ordinaryStringSingleChar: String = "z"

// Multi-segment interpolation, including segments that are themselves
// single characters, resolves correctly -- this used to fail because each
// single-character segment was unconditionally locked to
// `ExpressibleByUnicodeScalarLiteral`/`ExpressibleByExtendedGraphemeClusterLiteral`
// regardless of context, long before the umbrella-protocol redesign for
// plain multi-character literals even applied.
func interpolationLocalName() -> UncheckedString<UInt8> { "world" }
let interpolationGreeting: UncheckedString<UInt8> =
  "hello, \(interpolationLocalName())!"

// `\x{hh}` inside an interpolated literal's segment -- the original
// motivating limitation for this whole redesign.
let interpolationWithEscape: UncheckedString<UInt8> =
  "Ren\x{e9} says: \(interpolationLocalName())"

