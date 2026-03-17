// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Osize -Xllvm -sil-disable-pass=GenericSpecializer -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// Regression test for an ODR violation in ConstantCapturePropagation.
//
// FixedWidthInteger.init?(_:radix:) contains an @_alwaysEmitIntoClient
// closure. ConstantCapturePropagation specializes it by propagating the
// constant radix into the body. The specialized function gets a mangled
// name encoding only the constant (radix=10), not the type substitution,
// and shared (linkonce_odr) linkage.
//
// Before the fix, the pass baked the concrete Self type into the body,
// so UInt8 and Int produced the same symbol with different bodies.
// The linker picked one — if UInt8's 1-byte body won, Int callers got
// garbage because _parseIntegerDigits wrote only 1 byte into an 8-byte
// result buffer.
//
// GenericSpecializer is disabled to isolate the bug — in practice this
// manifests in multi-module builds where GenericSpecializer doesn't run
// on the @_alwaysEmitIntoClient closure before CCP does.

@inline(never)
func parseU8(_ s: String) -> UInt8? {
    return UInt8(s, radix: 10)
}

@inline(never)
func parseInt(_ s: String) -> Int? {
    return Int(s, radix: 10)
}

@inline(never)
func parseInt32(_ s: String) -> Int32? {
    return Int32(s, radix: 10)
}

// CHECK: UInt8: Optional(42)
print("UInt8: \(parseU8("42") as Any)")

// CHECK: Int: Optional(42)
print("Int: \(parseInt("42") as Any)")

// CHECK: Int32: Optional(42)
print("Int32: \(parseInt32("42") as Any)")
