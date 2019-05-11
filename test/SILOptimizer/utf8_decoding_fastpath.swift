// RUN: %target-swift-frontend -Xllvm -swiftmergefunc-threshold=0 -parse-as-library -O -target-cpu core2 -emit-ir  %s | %FileCheck %s
// REQUIRES: optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// good code for various ways to create a String from a Sequence of UTF-8
// bytes for which a fast path exists.

// Please note: this test targets "core2" to ensure consistent output
// on all x86 host processors.

@inline(never)
func blackhole<T>(_ value: T) {}

// UnsafeBufferPointer<UInt8>
// ==========================
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}s22utf8_decoding_fastpath15decodeUBPAsUTF8ySSSRys5UInt8VGF{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: ret
public func decodeUBPAsUTF8(_ ptr: UnsafeBufferPointer<UInt8>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeMutableBufferPointer<UInt8>
// =================================
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}s22utf8_decoding_fastpath16decodeUMBPAsUTF8ySSSrys5UInt8VGF{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: ret
public func decodeUMBPAsUTF8(_ ptr: UnsafeMutableBufferPointer<UInt8>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Array<UInt8>
// ============
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeArrayAsUTF8{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: ret
public func decodeArrayAsUTF8(_ ptr: [UInt8]) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeRawBufferPointer
// ======================
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeURBPAsUTF8{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: ret
public func decodeURBPAsUTF8(_ ptr: UnsafeRawBufferPointer) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeMutableRawBufferPointer
// =============================
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeUMRBPAsUTF8{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: ret
public func decodeUMRBPAsUTF8(_ ptr: UnsafeMutableRawBufferPointer) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// String.UTF8View
// ===============
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeStringUTF8ViewAs{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: br
public func decodeStringUTF8ViewAsUTF8(_ ptr: String.UTF8View) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Substring.UTF8View
// ==================
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeSubstringUTF8ViewAs{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: br
public func decodeSubstringUTF8ViewAsUTF8(_ ptr: Substring.UTF8View) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Slice<UBP>
// ==========
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeUBPSliceAsUTF8{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK-LAST: br
public func decodeUBPSliceAsUTF8(_ ptr: Slice<UnsafeBufferPointer<UInt8>>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Slice<URBP>
// ===========
// CHECK-LABEL: define {{.*}}swiftcc {{.*}}decodeURBPSliceAsUTF8{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: {{.*}} = call swiftcc {{.*}} @"$sSS18_fromUTF8Repairing{{.*}}
// CHECK-NOT: _fromCodeUnits
// CHECK: ret
public func decodeURBPSliceAsUTF8(_ ptr: Slice<UnsafeBufferPointer<UInt8>>) -> String {
  blackhole("foo") // otherwise it just jumps into the Slice<UBP> version
  return String(decoding: ptr, as: Unicode.UTF8.self)
}
