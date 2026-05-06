// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Osize -Xllvm -sil-disable-pass=GenericSpecializer -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

// Regression test for an ODR violation in ConstantCapturePropagation.
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
