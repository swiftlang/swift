// RUN: %target-run-stdlib-swift | FileCheck %s

// XFAIL: linux

import Foundation
import Swift
// ==== Tests =====

func hex(x: UInt64) -> String { return String(x, radix:16) }

func hexAddrVal<T>(x: T) -> String {
  return "@0x" + hex(UInt64(unsafeBitCast(x, Word.self)))
}

func hexAddr(x: AnyObject?) -> String {
  if let owner? = x {
    if let y? = owner as? _StringBuffer._Storage.Storage {
      return ".Native\(hexAddrVal(y))"
    }
    if let y? = owner as? NSString {
      return ".Cocoa\(hexAddrVal(y))"
    }
    else {
      return "?Uknown?\(hexAddrVal(owner))"
    }
  }
  return "null"
}

func repr(x: NSString) -> String {
  return "\(NSStringFromClass(object_getClass(x)))\(hexAddrVal(x)) = \"\(x)\""
}

func repr(x: _StringCore) -> String {
  if x.hasContiguousStorage {
    if let b? = x.nativeBuffer {
    var offset = x.elementWidth == 2
      ? UnsafeMutablePointer(b.start) - x.startUTF16
      : UnsafeMutablePointer(b.start) - x.startASCII
      return "Contiguous(owner: "
      + "\(hexAddr(x._owner))[\(offset)...\(x.count + offset)]"
      + ", capacity = \(b.capacity))"
    }
    return "Contiguous(owner: \(hexAddr(x._owner)), count: \(x.count))"
  }
  else if let b2? = x.cocoaBuffer {
    return "Opaque(buffer: \(hexAddr(b2))[0...\(x.count)])"
  }
  return "?????"
}

func repr(x: String) -> String {
  return "String(\(repr(x._core))) = \"\(x)\""
}

// CHECK: Testing
println("Testing...")

//===--------- Native Strings ---------===

// Force the string literal representation into a Native, heap-allocated buffer
var nsb = "🏂☃❅❆❄︎⛄️❄️"
// CHECK-NEXT: Hello, snowy world: 🏂☃❅❆❄︎⛄️❄️
println("Hello, snowy world: \(nsb)")
// CHECK-NEXT: String(Contiguous(owner: null, count: 11))
println("  \(repr(nsb))")

var empty = String()
// CHECK-NEXT: These are empty: <>
println("These are empty: <\(empty)>")
// CHECK-NEXT: String(Contiguous(owner: null, count: 0))
println("  \(repr(empty))")


//===--------- non-ASCII ---------===

func nonASCII() {
  // Cocoa stores non-ASCII in a UTF-16 buffer
  // Code units in each character: 2 1 1 1 2 2 2
  // Offset of each character:     0 2 3 4 5 7 9 11
  var nsUTF16 = NSString(UTF8String: "🏂☃❅❆❄︎⛄️❄️")!
  // CHECK-NEXT: has UTF-16: true
  println("has UTF-16: \(CFStringGetCharactersPtr(unsafeBitCast(nsUTF16, CFString.self)) != nil)")

  // CHECK: --- UTF-16 basic round-tripping ---
  println("--- UTF-16 basic round-tripping ---")

  // check that no extraneous objects are created
  // CHECK-NEXT: __NSCFString@[[utf16address:[x0-9a-f]+]] = "🏂☃❅❆❄︎⛄️❄️"
  println("  \(repr(nsUTF16))")

  // CHECK-NEXT: String(Contiguous(owner: .Cocoa@[[utf16address]], count: 11))
  var newNSUTF16 = nsUTF16 as String
  println("  \(repr(newNSUTF16))")

  // CHECK-NEXT: __NSCFString@[[utf16address]] = "🏂☃❅❆❄︎⛄️❄️"
  var nsRoundTripUTF16 = newNSUTF16 as NSString
  println("  \(repr(nsRoundTripUTF16))")

  // CHECK: --- UTF-16 slicing ---
  println("--- UTF-16 slicing ---")

  // Slicing the String does not allocate
  // CHECK-NEXT: String(Contiguous(owner: .Cocoa@[[utf16address]], count: 6))
  let i2 = advance(newNSUTF16.startIndex, 2)
  let i8 = advance(newNSUTF16.startIndex, 6)
  println("  \(repr(newNSUTF16[i2..<i8]))")

  // Representing a slice as an NSString requires a new object
  // CHECK-NOT: NSString@[[utf16address]] = "❅❆❄︎⛄️"
  // CHECK-NEXT: _NSContiguousString@[[nsContiguousStringAddress:[x0-9a-f]+]] = "❅❆❄︎⛄️"
  var nsSliceUTF16 = newNSUTF16[i2..<i8] as NSString
  println("  \(repr(nsSliceUTF16))")

  // Check that we can recover the original buffer
  // CHECK-NEXT: String(Contiguous(owner: .Cocoa@[[utf16address]], count: 6))
  println("  \(repr(nsSliceUTF16 as String))")
}
nonASCII()

//===--------- ASCII ---------===

func ascii() {
  // Cocoa stores ASCII in a buffer of bytes.  This is an important case
  // because it doesn't provide a contiguous array of UTF-16, so we'll be
  // treating it as an opaque NSString.
  var nsASCII = NSString(UTF8String: "foobar")!
  // CHECK-NEXT: has UTF-16: false
  println("has UTF-16: \(CFStringGetCharactersPtr(unsafeBitCast(nsASCII, CFString.self)) != nil)")

  // CHECK: --- ASCII basic round-tripping ---
  println("--- ASCII basic round-tripping ---")

  // CHECK-NEXT: [[nsstringclass:(__NSCFString|NSTaggedPointerString)]]@[[asciiaddress:[x0-9a-f]+]] = "foobar"
  println("  \(repr(nsASCII))")

  // CHECK-NEXT NO: String(Opaque(buffer: @[[asciiaddress]][0...6]))
  var newNSASCII = nsASCII as String
  // println("  \(repr(newNSASCII))")

  // CHECK-NEXT: [[nsstringclass]]@[[asciiaddress]] = "foobar"
  var nsRoundTripASCII = newNSASCII as NSString
  println("  \(repr(nsRoundTripASCII))")

  // CHECK: --- ASCII slicing ---
  println("--- ASCII slicing ---")

  let i3 = advance(newNSASCII.startIndex, 3)
  let i6 = advance(newNSASCII.startIndex, 6)
  
  // Slicing the String
  println("  \(repr(newNSASCII[i3..<i6]))")

  // Representing a slice as an NSString
  var nsSliceASCII = newNSASCII[i3..<i6] as NSString
  println("  \(repr(nsSliceASCII))")

  // Round-tripped back to Swift
  println("  \(repr(nsSliceASCII as String))")
}
ascii()

//===-------- Literals --------===

// String literals default to UTF-16.

// CHECK: --- Literals ---
println("--- Literals ---")

// CHECK-NEXT: String(Contiguous(owner: null, count: 6)) = "foobar"
// CHECK-NEXT: true
var asciiLiteral: String = "foobar"
println("  \(repr(asciiLiteral))")
println("  \(asciiLiteral._core.isASCII)")

// CHECK-NEXT: String(Contiguous(owner: null, count: 11)) = "🏂☃❅❆❄︎⛄️❄️"
// CHECK-NEXT: false
var nonASCIILiteral: String = "🏂☃❅❆❄︎⛄️❄️"
println("  \(repr(nonASCIILiteral))")
println("  \(!asciiLiteral._core.isASCII)")

// ===------- Appending -------===

// These tests are in NewStringAppending.swift.

// ===---------- Comparison --------===

var s = "ABCDEF"
var s1 = s + "G"

// CHECK-NEXT: true
println("\(s) == \(s) => \(s == s)")

// CHECK-NEXT: false
println("\(s) == \(s1) => \(s == s1)")

// CHECK-NEXT: true
let abcdef: String = "ABCDEF"
println("\(s) == \"\(abcdef)\" => \(s == abcdef)")

let so: String = "so"
let sox: String = "sox"
let tocks: String = "tocks"

// CHECK-NEXT: false
println("so < so => \(so < so)")
// CHECK-NEXT: true
println("so < sox => \(so < sox)")
// CHECK-NEXT: true
println("so < tocks => \(so < tocks)")
// CHECK-NEXT: true
println("sox < tocks => \(sox < tocks)")

let qqq = nonASCIILiteral.hasPrefix("🏂☃")
let rrr = nonASCIILiteral.hasPrefix("☃")
let zz = (
  nonASCIILiteral.hasPrefix("🏂☃"), nonASCIILiteral.hasPrefix("☃"),
  nonASCIILiteral.hasSuffix("⛄️❄️"), nonASCIILiteral.hasSuffix("☃"))

// CHECK-NEXT: <true, false, true, false>
println("<\(zz.0), \(zz.1), \(zz.2), \(zz.3)>")

// ===---------- Interpolation --------===

// CHECK-NEXT: {{.*}}"interpolated: foobar 🏂☃❅❆❄︎⛄️❄️ 42 3.14 true"
s = "interpolated: \(asciiLiteral) \(nonASCIILiteral) \(42) \(3.14) \(true)"
println("\(repr(s))")

// ===---------- Done --------===
// CHECK-NEXT: Done.
println("Done.")

