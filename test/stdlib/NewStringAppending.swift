// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test
//
// Parts of this test depend on memory allocator specifics.  The test
// should be rewritten soon so it doesn't expose legacy components
// like OpaqueString anyway, so we can just disable the failing
// configuration
//
// Memory allocator specifics also vary across platforms.
// REQUIRES: CPU=x86_64, OS=macosx

import Foundation
import Swift

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

// ===------- Appending -------===

// CHECK-LABEL: --- Appending ---
print("--- Appending ---")

var s = "⓪" // start non-empty

// To make this test independent of the memory allocator implementation,
// explicitly request initial capacity.
s.reserveCapacity(16)

// CHECK-NEXT: String(Native(owner: @[[storage0:[x0-9a-f]+]], count: 3, capacity: 23)) = "⓪"
print("\(repr(s))")

// CHECK-NEXT: String(Native(owner: @[[storage0]], count: 4, capacity: 23)) = "⓪1"
s += "1"
print("\(repr(s))")

// CHECK-NEXT: String(Native(owner: @[[storage0]], count: 10, capacity: 23)) = "⓪1234567"
s += "234567"
print("\(repr(s))")

// CHECK-NEXT: String(Native(owner: @[[storage0]], count: 11, capacity: 23)) = "⓪12345678"
// CHECK-NOT: @[[storage0]],
s += "8"
print("\(repr(s))")

// CHECK-NEXT: String(Native(owner: @[[storage0]], count: 18, capacity: 23)) = "⓪123456789012345"
s += "9012345"
print("\(repr(s))")

// -- expect a reallocation here
// CHECK-LABEL: (expecting reallocation)
print("(expecting reallocation)")

// Appending more than the next level of capacity only takes as much
// as required.  I'm not sure whether this is a great idea, but the
// point is to prevent huge amounts of fragmentation when a long
// string is appended to a short one.  The question, of course, is
// whether more appends are coming, in which case we should give it
// more capacity.  It might be better to always grow to a multiple of
// the current capacity when the capacity is exceeded.

// CHECK-NEXT: String(Native(owner: @[[storage1:[x0-9a-f]+]], count: 54, capacity: 55))
// CHECK-NOT: @[[storage1]],
s += s + s
print("\(repr(s))")

// CHECK-NEXT: String(Native(owner: @[[storage1]], count: 55, capacity: 55))
s += "C"
print("\(repr(s))")

// -- expect a reallocation here
// CHECK-LABEL: (expecting second reallocation)
print("(expecting second reallocation)")

// CHECK-NEXT: String(Native(owner: @[[storage2:[x0-9a-f]+]], count: 56, capacity: 119))
// CHECK-NOT: @[[storage1]],
s += "C"
print("\(repr(s))")

// -- expect a reallocation here
// CHECK-LABEL: (expecting third reallocation)
print("(expecting third reallocation)")

// CHECK-NEXT: String(Native(owner: @[[storage3:[x0-9a-f]+]], count: 72, capacity: 119))
// CHECK-NOT: @[[storage2]],
s += "1234567890123456"
print("\(repr(s))")

var s1 = s

// CHECK-NEXT: String(Native(owner: @[[storage3]], count: 72, capacity: 119))
print("\(repr(s1))")

/// The use of later buffer capacity by another string forces
/// reallocation; however, the original capacity is kept by intact

// CHECK-LABEL: (expect copy to trigger reallocation without growth)
print("(expect copy to trigger reallocation without growth)")

// CHECK-NEXT: String(Native(owner: @[[storage4:[x0-9a-f]+]], count: 73, capacity: 87)) = "{{.*}}X"
// CHECK-NOT: @[[storage3]],
s1 += "X"
print("\(repr(s1))")

/// The original copy is left unchanged

// CHECK-NEXT: String(Native(owner: @[[storage3]], count: 72, capacity: 119))
print("\(repr(s))")

/// Appending to an empty string re-uses the RHS

// CHECK-NEXT: @[[storage3]],
var s2 = String()
s2 += s
print("\(repr(s2))")
