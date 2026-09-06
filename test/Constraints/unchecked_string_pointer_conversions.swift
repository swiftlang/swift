// RUN: %target-typecheck-verify-swift -disable-availability-checking

// `UncheckedString<Element>` can be passed directly where a C function
// expects `UnsafePointer<Element>` or `UnsafeRawPointer`, mirroring `String`'s
// own conversion to `UnsafePointer<Int8>`/`UnsafePointer<UInt8>`, but
// generalized to the string's actual `Element` type instead of a fixed byte
// width.

func takesUInt8Pointer(_ p: UnsafePointer<UInt8>) {}
func takesUInt16Pointer(_ p: UnsafePointer<UInt16>) {}
func takesUInt32Pointer(_ p: UnsafePointer<UInt32>) {}
func takesCCharPointer(_ p: UnsafePointer<CChar>) {}
func takesRawPointer(_ p: UnsafeRawPointer) {}

func passUInt8(_ s: UncheckedString<UInt8>) {
  takesUInt8Pointer(s)
  takesRawPointer(s)
}

func passUInt16(_ s: UncheckedString<UInt16>) {
  takesUInt16Pointer(s)
  takesRawPointer(s)
}

func passUInt32(_ s: UncheckedString<UInt32>) {
  takesUInt32Pointer(s)
  takesRawPointer(s)
}

func passCChar(_ s: UncheckedString<CChar>) {
  takesCCharPointer(s)
}

// A string literal can be passed directly, exactly like a `String` literal.
takesUInt8Pointer("hello" as UncheckedString<UInt8>)

// The pointee type must match the string's `Element` exactly -- there's no
// implicit widening/narrowing the way there is for ordinary integer
// conversions elsewhere in the language.
func mismatchedElement(_ s: UncheckedString<UInt16>) {
  takesUInt8Pointer(s)
  // expected-error@-1 {{cannot convert value of type 'UnsafePointer<UInt16>' to expected argument type 'UnsafePointer<UInt8>'}}
  // expected-note@-2 {{arguments to generic parameter 'Pointee' ('UInt16' and 'UInt8') are expected to be equal}}
}

// `UncheckedString<UInt8>` is not automatically `UnsafePointer<CChar>` --
// `CChar` is `Int8`, a distinct type from `UInt8`, even though both are 8
// bits wide.
func uint8IsNotCChar(_ s: UncheckedString<UInt8>) {
  takesCCharPointer(s)
  // expected-error@-1 {{cannot convert value of type 'UnsafePointer<UInt8>' to expected argument type 'UnsafePointer<CChar>' (aka 'UnsafePointer<Int8>')}}
  // expected-note@-2 {{arguments to generic parameter 'Pointee' ('UInt8' and 'CChar' (aka 'Int8')) are expected to be equal}}
}

// `String` itself is unaffected by any of the above.
func stringStillWorks(_ s: String) {
  takesUInt8Pointer(s)
}
