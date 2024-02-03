// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// The GenericSignatureBuilder did not diagnose the first two.

// CHECK-LABEL: .G1@
// CHECK-NEXT: Generic signature: <T where T : Decodable, T : Encodable, T : FixedWidthInteger, T : UnsignedInteger>
struct G1<T : BinaryInteger & FixedWidthInteger & UnsignedInteger & Codable> {}

// CHECK-LABEL: .G2@
// CHECK-NEXT: Generic signature: <T where T : Decodable, T : Encodable, T : FixedWidthInteger, T : UnsignedInteger>
struct G2<T> where T : BinaryInteger & FixedWidthInteger & UnsignedInteger & Codable {}

// CHECK-LABEL: .G3@
// CHECK-NEXT: Generic signature: <T where T : Decodable, T : Encodable, T : FixedWidthInteger, T : UnsignedInteger>
struct G3<T> where T : BinaryInteger, T : FixedWidthInteger, T : UnsignedInteger & Codable {}

// CHECK-LABEL: .G1a@
// CHECK-NEXT: Generic signature: <T where T : FixedWidthInteger, T : UnsignedInteger>
struct G1a<T : BinaryInteger & FixedWidthInteger & UnsignedInteger> {}

// CHECK-LABEL: .G2a@
// CHECK-NEXT: Generic signature: <T where T : FixedWidthInteger, T : UnsignedInteger>
struct G2a<T> where T : BinaryInteger & FixedWidthInteger & UnsignedInteger {}

