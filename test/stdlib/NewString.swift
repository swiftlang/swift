// RUN: %swift -i -parse-stdlib %s | FileCheck %s

import Foundation

// === Make various things string-interpolable ===
import swift

extension String {
  init(x: ContiguousUTF16Slice) {
    var a: NSString = x
    self = String(a)
  }
  init(x: OpaqueUTF16Slice) {
    var a: NSString = x
    self = String(a)
  }
  init(x: NewString) {
    switch x.representation {
    case .Contiguous(var rep):
      self = String(rep)
    case .Opaque(var rep):
      self = String(rep)
    }
  }
}

// ==== Tests =====

func hexAddr(x: DynamicLookup) -> String {
  return hexAddr(Builtin.addressof(&x))
}

func hexAddr(x: Builtin.RawPointer) -> String {
  var p = UnsafePointer<UInt64>(x)
  return "@0x" + p.get().format('x', "")
}

func repr(x: NSString) -> String {
  return "NSString\(hexAddr(x)) = \"\(x)\""
}

func repr(x: OpaqueUTF16Slice) -> String {
  return "OpaqueUTF16Slice("
         + "buffer: \(hexAddr(Builtin.addressof(&x.buffer))),"
         + " \(x.range.startIndex())..\(x.range.endIndex()))"
}

func repr(x: ContiguousUTF16Slice) -> String {
  return "ContiguousUTF16Slice(owner: \(hexAddr(x.owner)), count: \(x.count))"
}

func repr(x: NewString) -> String {
  switch x.representation {
  case .Contiguous(var rep):
    return "NewString(\(repr(rep)))"
  case .Opaque(var rep):
    return "NewString(\(repr(rep)))"
  }
}

// CHECK: Testing
println("Testing...")

//===--------- non-ASCII ---------===

// Cocoa stores non-ASCII in a UTF16 buffer
var nsUTF16 = NSString(withUTF8String: "🏂☃❅❆❄︎⛄️❄️")
// CHECK-NEXT: false
println("\(CFStringGetCharactersPtr(nsUTF16).isNull())")

// CHECK-NEXT: NSString@[[utf16address:[x0-9a-f]+]] = "🏂☃❅❆❄︎⛄️❄️"
println("  \(repr(nsUTF16))")

// CHECK-NEXT: NewString(ContiguousUTF16Slice(owner: @[[utf16address]]
var newNSUTF16 = NewString(nsUTF16)
println("  \(repr(newNSUTF16))")

// CHECK-NEXT: NSString@[[utf16address]] = "🏂☃❅❆❄︎⛄️❄️"
var nsRoundTripUTF16: NSString = newNSUTF16
println("  \(repr(nsRoundTripUTF16))")

//===--------- ASCII ---------===

// Cocoa stores ASCII in a buffer of bytes
var nsASCII = NSString(withUTF8String: "foobar")
// CHECK-NEXT: true
println("\(CFStringGetCharactersPtr(nsASCII).isNull())")

// CHECK-NEXT: NSString@[[asciiaddress:[x0-9a-f]+]] = "foobar"
println("  \(repr(nsASCII))")

// CHECK-NEXT: NewString(OpaqueUTF16Slice(buffer: @[[asciiaddress]], 0..6))
var newNSASCII = NewString(nsASCII)
println("  \(repr(newNSASCII))")

// CHECK-NEXT: NSString@[[asciiaddress]] = "foobar"
var nsRoundTripASCII: NSString = newNSASCII
println("  \(repr(nsRoundTripASCII))")

// CHECK-NEXT: Done.
println("Done.")

