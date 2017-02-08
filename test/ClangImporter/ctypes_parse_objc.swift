// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import ctypes
import CoreGraphics
import Foundation
import CoreFoundation

var cgPointVar: CGPoint

// FIXME: Import arrays as real array-looking things.

func testArrays() {
  let fes: NSFastEnumerationState
  var ulong: CUnsignedLong
  var pulong: UnsafeMutablePointer<CUnsignedLong>

  ulong = fes.state
  pulong = fes.mutationsPtr
  ulong = fes.extra.0
  ulong = fes.extra.1
  ulong = fes.extra.2
  ulong = fes.extra.3
  ulong = fes.extra.4
  _ = ulong; _ = pulong
}

// FIXME: Import pointers to opaque types as unique types.

func testPointers() {
  let cfstr: CFString?
  _ = cfstr as CFTypeRef?
}

// Ensure that imported structs can be extended, even if typedef'ed on the C
// side.

extension NSFastEnumerationState {
  func reset() {}
}

extension CGRectTy {
  init(x: Double, y: Double, w: Double, h: Double) {
    origin.x = CGFloat(x)
    origin.y = CGFloat(y)
    size.width = CGFloat(w)
    size.height = CGFloat(h)
  }
}

extension CGRect {
  func printAsX11Geometry() {
    print("\(size.width)x\(size.height)+\(origin.x)+\(origin.y)", terminator: "")
  }
}

func testImportMacTypes() {
  // Test that we import integer and floating-point types as swift stdlib
  // types.
  var a : UInt32 = UInt32_test
  a = a + 1

  var b : Float64 = Float64_test
  b = b + 1

  var t9_unqual : Float32 = Float32_test
  var t10_unqual : Float64 = Float64_test

  var t9_qual : ctypes.Float32 = 0.0  // expected-error {{no type named 'Float32' in module 'ctypes'}}
  var t10_qual : ctypes.Float64 = 0.0 // expected-error {{no type named 'Float64' in module 'ctypes'}}
}

func testImportCFTypes() {
  let t1_unqual: Int = CFIndex_test
  _ = t1_unqual as CoreFoundation.CFIndex
}

func testImportSEL() {
  var t1 : SEL // expected-error {{use of undeclared type 'SEL'}} {{12-15=Selector}}
  var t2 : ctypes.SEL // expected-error {{no type named 'SEL' in module 'ctypes'}}
}

func testStructDefaultInit() {
  let _ = NonNilableReferences() // expected-error{{missing argument}}
}
