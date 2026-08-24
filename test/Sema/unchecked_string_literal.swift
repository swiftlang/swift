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
