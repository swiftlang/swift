// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s
// REQUIRES: optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// good code for various ways to create a String from a Sequence of UTF-8
// bytes for which a fast path exists.

// Please note: this test targets "core2" to ensure consistent output
// on all x86 host processors.

@inline(never)
func blackhole<T>(_ value: T) {}

// UnsafeBufferPointer<UInt8>
//
// CHECK-LABEL: sil {{.*}}decodeUBPAsUTF{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function {{.*}}decodeUBPAsUTF
public func decodeUBPAsUTF8(_ ptr: UnsafeBufferPointer<UInt8>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeMutableBufferPointer<UInt8>
//
// CHECK-LABEL: sil {{.*}}decodeUMBPAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeUMBPAsUTF8
public func decodeUMBPAsUTF8(_ ptr: UnsafeMutableBufferPointer<UInt8>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Array<UInt8>
//
// CHECK-LABEL: sil {{.*}}decodeArrayAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeArrayAsUTF8
public func decodeArrayAsUTF8(_ ptr: [UInt8]) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeRawBufferPointer
//
// CHECK-LABEL: sil {{.*}}decodeURBPAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeURBPAsUTF8
public func decodeURBPAsUTF8(_ ptr: UnsafeRawBufferPointer) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// UnsafeMutableRawBufferPointer
//
// CHECK-LABEL: sil {{.*}}decodeUMRBPAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeUMRBPAsUTF8
public func decodeUMRBPAsUTF8(_ ptr: UnsafeMutableRawBufferPointer) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// String.UTF8View
//
// CHECK-LABEL: sil {{.*}}decodeStringUTF8ViewAs{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-DAG:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-DAG:   function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeStringUTF8ViewAs
public func decodeStringUTF8ViewAsUTF8(_ ptr: String.UTF8View) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Substring.UTF8View
//
// NOTE: withContiguousStorageIfAvailable is not currently inlined at the SIL
// level, so we have to disable the UTF8Repairing check :-(
//
// CHECK-LABEL: sil {{.*}}decodeSubstringUTF8ViewAs{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-DAG:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// xCHECK-DAG:   function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeSubstringUTF8ViewAs
public func decodeSubstringUTF8ViewAsUTF8(_ ptr: Substring.UTF8View) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Slice<UBP>
//
// CHECK-LABEL: sil {{.*}}decodeUBPSliceAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeUBPSliceAsUTF8
public func decodeUBPSliceAsUTF8(_ ptr: Slice<UnsafeBufferPointer<UInt8>>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

// Slice<URBP>
//
// CHECK-LABEL: sil {{.*}}decodeURBPSliceAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeURBPSliceAsUTF8
public func decodeURBPSliceAsUTF8(_ ptr: Slice<UnsafeBufferPointer<UInt8>>) -> String {
  return String(decoding: ptr, as: Unicode.UTF8.self)
}

public struct CustomContiguousCollection: Collection {
  let storage: [UInt8]
  public typealias Index = Int
  public typealias Element = UInt8

  public init(_ bytes: [UInt8]) { self.storage = bytes }
  public subscript(position: Int) -> Element { self.storage[position] }
  public var startIndex: Index { 0 }
  public var endIndex: Index { storage.count }
  public func index(after i: Index) -> Index { i+1 }

  @inline(__always)
  public func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R? {
    try storage.withContiguousStorageIfAvailable(body)
  }
}
public struct CustomNonContiguousCollection: Collection {
  let storage: [UInt8]
  public typealias Index = Int
  public typealias Element = UInt8

  public init(_ bytes: [UInt8]) { self.storage = bytes }
  public subscript(position: Int) -> Element { self.storage[position] }
  public var startIndex: Index { 0 }
  public var endIndex: Index { storage.count }
  public func index(after i: Index) -> Index { i+1 }

  @inline(__always)
  public func withContiguousStorageIfAvailable<R>(
    _ body: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R? {
    nil
  }
}

// CustomContiguousCollection
//
// CHECK-LABEL: sil {{.*}}decodeCustomContiguousAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK:       function_ref {{.*}}_fromUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-LABEL: end sil function{{.*}}decodeCustomContiguousAsUTF8
public func decodeCustomContiguousAsUTF8(_ c: CustomContiguousCollection) -> String {
  return String(decoding: c, as: UTF8.self)
}

// CustomNonContiguousCollection
//
// CHECK-LABEL: sil {{.*}}decodeCustomNonContiguousAsUTF8{{.*}} : $@convention
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-NOT:   function_ref {{.*}}_fromUTF8Repairing
// CHECK:       function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-NOT:   function_ref {{.*}}_fromCodeUnits
// CHECK-NOT:   function_ref {{.*}}_fromUTF8Repairing
// CHECK-LABEL: end sil function{{.*}}decodeCustomNonContiguousAsUTF8
public func decodeCustomNonContiguousAsUTF8(_ c: CustomNonContiguousCollection) -> String {
  return String(decoding: c, as: UTF8.self)
}

// UTF-16
//
// NOTE: The SIL optimizer cannot currently fold away a (UTF16.self ==
// UTF8.self) metatype comparison, so we have to disabel the check-not for UTF-8
// construction :-(
//
// CHECK-LABEL: sil {{.*}}decodeUTF16{{.*}} : $@convention
// xCHECK-NOT:  function_ref {{.*}}_fromUTF8Repairing
// xCHECK-NOT:  function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK:       function_ref {{.*}}_fromCodeUnits
// xCHECK-NOT:  function_ref {{.*}}_fromUTF8Repairing
// xCHECK-NOT:  function_ref {{.*}}_fromNonContiguousUnsafeBitcastUTF8Repairing
// CHECK-LABEL: end sil function{{.*}}decodeUTF16
public func decodeUTF16(_ c: Array<UInt16>) -> String {
  return String(decoding: c, as: UTF16.self)
}
