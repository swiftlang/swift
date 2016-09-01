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

func hexAddrVal<T>(_ x: T) -> String {
  return "@0x" + String(UInt64(unsafeBitCast(x, to: Int.self)), radix: 16)
}

func hexAddr(_ x: AnyObject?) -> String {
  if let owner = x {
    if let y = owner as? _StringBuffer._Storage.Storage {
      return ".native\(hexAddrVal(y))"
    }
    if let y = owner as? NSString {
      return ".cocoa\(hexAddrVal(y))"
    }
    else {
      return "?Uknown?\(hexAddrVal(owner))"
    }
  }
  return "nil"
}

func repr(_ x: NSString) -> String {
  return "\(NSStringFromClass(object_getClass(x)))\(hexAddr(x)) = \"\(x)\""
}

func repr(_ x: _StringCore) -> String {
  if x.hasContiguousStorage {
    if let b = x.nativeBuffer {
    var offset = x.elementWidth == 2
      ? b.start - UnsafeMutableRawPointer(x.startUTF16)
      : b.start - UnsafeMutableRawPointer(x.startASCII)
      return "Contiguous(owner: "
      + "\(hexAddr(x._owner))[\(offset)...\(x.count + offset)]"
      + ", capacity = \(b.capacity))"
    }
    return "Contiguous(owner: \(hexAddr(x._owner)), count: \(x.count))"
  }
  else if let b2 = x.cocoaBuffer {
    return "Opaque(buffer: \(hexAddr(b2))[0...\(x.count)])"
  }
  return "?????"
}

func repr(_ x: String) -> String {
  return "String(\(repr(x._core))) = \"\(x)\""
}


// ===------- Appending -------===

// CHECK: --- Appending ---
print("--- Appending ---")

var s = "⓪" // start non-empty

// To make this test independent of the memory allocator implementation,
// explicitly request initial capacity.
s.reserveCapacity(8)

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer0:[x0-9a-f]+]][0...2], capacity = 8)) = "⓪1"
s += "1"
print("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer1:[x0-9a-f]+]][0...8], capacity = 8)) = "⓪1234567"
s += "234567"
print("\(repr(s))")

// -- expect a reallocation here

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer2:[x0-9a-f]+]][0...9], capacity = 16)) = "⓪12345678"
// CHECK-NOT: .native@[[buffer1]]
s += "8"
print("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer2]][0...16], capacity = 16)) = "⓪123456789012345"
s += "9012345"
print("\(repr(s))")

// -- expect a reallocation here

// Appending more than the next level of capacity only takes as much
// as required.  I'm not sure whether this is a great idea, but the
// point is to prevent huge amounts of fragmentation when a long
// string is appended to a short one.  The question, of course, is
// whether more appends are coming, in which case we should give it
// more capacity.  It might be better to always grow to a multiple of
// the current capacity when the capacity is exceeded.

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer3:[x0-9a-f]+]][0...48], capacity = 48))
// CHECK-NOT: .native@[[buffer2]]
s += s + s
print("\(repr(s))")

// -- expect a reallocation here

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer4:[x0-9a-f]+]][0...49], capacity = 96))
// CHECK-NOT: .native@[[buffer3]]
s += "C"
print("\(repr(s))")

/// An additional reference to the same buffer doesn't, by itself,
/// impede the use of available capacity
var s1 = s

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer4]][0...50], capacity = 96))
s += "F"
print("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .native@[[buffer4]][0...49], capacity = 96))
print("\(repr(s1))")

/// The use of later buffer capacity by another string forces
/// reallocation

// CHECK-NEXT: String{{.*}} = {{.*}}X"
// CHECK-NOT: .native@[[buffer4]]
s1 += "X"
print("\(repr(s1))")

/// Appending to an empty string re-uses the RHS

// CHECK-NEXT: .native@[[buffer4]]
var s2 = String()
s2 += s
print("\(repr(s2))")
