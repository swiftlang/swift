// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import Swift

// ==== Tests =====

func hex(_ x: UInt64) -> String { return String(x, radix:16) }

func hexAddrVal<T>(_ x: T) -> String {
  return "@0x" + hex(UInt64(unsafeBitCast(x, to: UInt.self)))
}

func repr(_ x: NSString) -> String {
  return "\(NSStringFromClass(object_getClass(x)!))\(hexAddrVal(x)) = \"\(x)\""
}

func repr(_ x: _StringRepresentation) -> String {
  switch x._form {
  case ._small:
    return """
      Small(count: \(x._count))
      """
  case ._cocoa(let object):
    return """
      Cocoa(\
      owner: \(hexAddrVal(object)), \
      count: \(x._count))
      """
  case ._native(let object):
    return """
      Native(\
      owner: \(hexAddrVal(object)), \
      count: \(x._count), \
      capacity: \(x._capacity))
      """
  case ._immortal(_):
    return """
      Unmanaged(count: \(x._count))
      """
  }
}

func repr(_ x: String) -> String {
  return "String(\(repr(x._classify()))) = \"\(x)\""
}

// CHECK-LABEL: Testing...
print("Testing...")

//===--------- Native Strings ---------===

// Force the string literal representation into a Native, heap-allocated buffer
var nsb = "🏂☃❅❆❄︎⛄️❄️"
// CHECK-NEXT: Hello, snowy world: 🏂☃❅❆❄︎⛄️❄️
print("Hello, snowy world: \(nsb)")
// CHECK-NEXT: String(Unmanaged(count: 31))
print("  \(repr(nsb))")

var empty = String()
// CHECK-NEXT: These are empty: <>
print("These are empty: <\(empty)>")
// CHECK-NEXT: String({{Unmanaged|Small}}(count: 0))
print("  \(repr(empty))")


//===--------- non-ASCII ---------===

func nonASCII() {
  // Cocoa stores non-ASCII in a UTF-16 buffer
  // Code units in each character: 2 1 1 1 2 2 2
  // Offset of each character:     0 2 3 4 5 7 9 11
  let nsUTF16 = NSString(utf8String: "🏂☃❅❆❄︎⛄️❄️")!
  // CHECK-NEXT: has UTF-16: true
  print("has UTF-16: \(CFStringGetCharactersPtr(unsafeBitCast(nsUTF16, to: CFString.self)) != nil)")

  // CHECK-LABEL: --- UTF-16 basic round-tripping ---
  print("--- UTF-16 basic round-tripping ---")

  // check that no extraneous objects are created
  // CHECK-NEXT: __NSCFString@[[utf16address:[x0-9a-f]+]] = "🏂☃❅❆❄︎⛄️❄️"
  print("  \(repr(nsUTF16))")

  // CHECK-NEXT: String(Cocoa(owner: @[[utf16address]], count: 11)) = "🏂☃❅❆❄︎⛄️❄️"
  let newNSUTF16 = nsUTF16 as String
  print("  \(repr(newNSUTF16))")

  // CHECK-NEXT: __NSCFString@[[utf16address]] = "🏂☃❅❆❄︎⛄️❄️"
  let nsRoundTripUTF16 = newNSUTF16 as NSString
  print("  \(repr(nsRoundTripUTF16))")

  // CHECK-LABEL: --- UTF-16 slicing ---
  print("--- UTF-16 slicing ---")

  // Slicing the String allocates a new buffer
  // CHECK-NOT: String(Native(owner: @[[utf16address]],
  // CHECK-NEXT: String(Native(owner: @[[sliceAddress:[x0-9a-f]+]], count: 18
  let i2 = newNSUTF16.index(newNSUTF16.startIndex, offsetBy: 2)
  let i8 = newNSUTF16.index(newNSUTF16.startIndex, offsetBy: 6)
  let slice = String(newNSUTF16[i2..<i8])
  print("  \(repr(slice))")

  // CHECK-LABEL: --- NSString slicing ---
  print("--- NSString slicing ---")

  // The storage of the slice implements NSString directly
  // CHECK-NOT: @[[utf16address]] = "❅❆❄︎⛄️"
  // CHECK-NEXT: {{.*}}StringStorage@[[sliceAddress]] = "❅❆❄︎⛄️"
  let nsSlice = slice as NSString
  print("  \(repr(nsSlice))")

  // Check that we can recover the original buffer
  // CHECK-NEXT: String(Native(owner: @[[sliceAddress]], count: 18
  print("  \(repr(nsSlice as String))")
}
nonASCII()

//===--------- ASCII ---------===

func ascii() {
  // Cocoa stores ASCII in a buffer of bytes.  This is an important case
  // because it doesn't provide a contiguous array of UTF-16, so we'll be
  // treating it as an opaque NSString.
  let nsASCII = NSString(utf8String: "foobar")!
  // CHECK-NEXT: has UTF-16: false
  print("has UTF-16: \(CFStringGetCharactersPtr(unsafeBitCast(nsASCII, to: CFString.self)) != nil)")
  print("has ASCII pointer: \(CFStringGetCStringPtr(unsafeBitCast(nsASCII, to: CFString.self), 0x0600) != nil)")
  print("has ASCII pointer: \(CFStringGetCStringPtr(unsafeBitCast(nsASCII, to: CFString.self), 0x08000100) != nil)")

  // CHECK-LABEL: --- ASCII basic round-tripping ---
  print("--- ASCII basic round-tripping ---")

  // CHECK-NEXT: [[nsstringclass:(__NSCFString|NSTaggedPointerString)]]@[[asciiaddress:[x0-9a-f]+]] = "foobar"
  print("  \(repr(nsASCII))")

  // CHECK-NEXT NO: String(Opaque(buffer: @[[asciiaddress]][0...6]))
  let newNSASCII = nsASCII as String
  // print("  \(repr(newNSASCII))")

  // CHECK-NEXT: [[nsstringclass]]@[[asciiaddress]] = "foobar"
  let nsRoundTripASCII = newNSASCII as NSString
  print("  \(repr(nsRoundTripASCII))")

  // CHECK-LABEL: --- ASCII slicing ---
  print("--- ASCII slicing ---")

  let i3 = newNSASCII.index(newNSASCII.startIndex, offsetBy: 3)
  let i6 = newNSASCII.index(newNSASCII.startIndex, offsetBy: 6)

  // Slicing the String
  print("  \(repr(String(newNSASCII[i3..<i6])))")

  // Representing a slice as an NSString
  let nsSliceASCII = newNSASCII[i3..<i6] as NSString
  print("  \(repr(nsSliceASCII))")

  // Round-tripped back to Swift
  print("  \(repr(nsSliceASCII as String))")
}
ascii()

//===-------- Literals --------===

// String literals default to UTF-16.

// CHECK-LABEL: --- Literals ---
print("--- Literals ---")

// CHECK-NEXT: String({{Unmanaged|Small}}(count: 6)) = "foobar"
// CHECK-NEXT: true
let asciiLiteral: String = "foobar"
print("  \(repr(asciiLiteral))")
print("  \(asciiLiteral._classify()._isASCII)")

// CHECK-NEXT: String(Unmanaged(count: 31)) = "🏂☃❅❆❄︎⛄️❄️"
// CHECK-NEXT: false
let nonASCIILiteral: String = "🏂☃❅❆❄︎⛄️❄️"
print("  \(repr(nonASCIILiteral))")
print("  \(nonASCIILiteral._classify()._isASCII)")

// ===------- Appending -------===

// These tests are in NewStringAppending.swift.

// ===---------- Comparison --------===

// CHECK-LABEL: --- Comparison ---
print("--- Comparison ---")

var s = "ABCDEF"
let s1 = s + "G"

// CHECK-NEXT: true
print("\(s) == \(s) => \(s == s)")

// CHECK-NEXT: false
print("\(s) == \(s1) => \(s == s1)")

// CHECK-NEXT: true
let abcdef: String = "ABCDEF"
print("\(s) == \"\(abcdef)\" => \(s == abcdef)")

let so: String = "so"
let sox: String = "sox"
let tocks: String = "tocks"

// CHECK-NEXT: false
print("so < so => \(so < so)")
// CHECK-NEXT: true
print("so < sox => \(so < sox)")
// CHECK-NEXT: true
print("so < tocks => \(so < tocks)")
// CHECK-NEXT: true
print("sox < tocks => \(sox < tocks)")

let qqq = nonASCIILiteral.hasPrefix("🏂☃")
let rrr = nonASCIILiteral.hasPrefix("☃")
let zz = (
  nonASCIILiteral.hasPrefix("🏂☃"), nonASCIILiteral.hasPrefix("☃"),
  nonASCIILiteral.hasSuffix("⛄️❄️"), nonASCIILiteral.hasSuffix("☃"))

// CHECK-NEXT: <true, false, true, false>
print("<\(zz.0), \(zz.1), \(zz.2), \(zz.3)>")

// ===---------- Interpolation --------===

// CHECK-NEXT: {{.*}}"interpolated: foobar 🏂☃❅❆❄︎⛄️❄️ 42 3.14 true"
s = "interpolated: \(asciiLiteral) \(nonASCIILiteral) \(42) \(3.14) \(true)"
print("\(repr(s))")

// ===---------- Done --------===
// CHECK-NEXT: Done.
print("Done.")

