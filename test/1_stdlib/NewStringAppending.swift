// RUN: %target-run-stdlib-swift | FileCheck %s
//
// Parts of this test depend on memory allocator specifics.  The test
// should be rewritten soon so it doesn't expose legacy components
// like OpaqueString anyway, so we can just disable the failing
// configuration
//
// Memory allocator specifics also vary across platorms.
// REQUIRES: CPU=x86_64, OS=macosx

import Foundation
import Swift

func hexAddrVal<T>(x: T) -> String {
  return "@0x" + String(UInt64(unsafeBitCast(x, Word.self)), radix: 16)
}

func hexAddr(x: AnyObject?) -> String {
  if let owner: AnyObject = x {
    if let y = owner as? _StringBuffer._Storage.Storage {
      return ".Native\(hexAddrVal(y))"
    }
    if let y = owner as? NSString {
      return ".Cocoa\(hexAddrVal(y))"
    }
    else {
      return "?Uknown?\(hexAddrVal(owner))"
    }
  }
  return "nil"
}

func repr(x: NSString) -> String {
  return "\(NSStringFromClass(object_getClass(x)))\(hexAddr(x)) = \"\(x)\""
}

func repr(x: _StringCore) -> String {
  if x.hasContiguousStorage {
    if let b = x.nativeBuffer {
    var offset = x.elementWidth == 2
      ? UnsafeMutablePointer(b.start) - x.startUTF16
      : UnsafeMutablePointer(b.start) - x.startASCII
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

func repr(x: String) -> String {
  return "String(\(repr(x._core))) = \"\(x)\""
}


// ===------- Appending -------===

// CHECK: --- Appending ---
println("--- Appending ---")

var s = "⓪" // start non-empty

// To make this test independent of the memory allocator implementation,
// explicitly request initial capacity.
s.reserveCapacity(8)

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer0:[x0-9a-f]+]][0...2], capacity = 8)) = "⓪1"
s += "1"
println("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer1:[x0-9a-f]+]][0...8], capacity = 8)) = "⓪1234567"
s += "234567"
println("\(repr(s))")

// -- expect a reallocation here

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer2:[x0-9a-f]+]][0...9], capacity = 16)) = "⓪12345678"
// CHECK-NOT: .Native@[[buffer1]]
s += "8"
println("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer2]][0...16], capacity = 16)) = "⓪123456789012345"
s += "9012345"
println("\(repr(s))")

// -- expect a reallocation here

// Appending more than the next level of capacity only takes as much
// as required.  I'm not sure whether this is a great idea, but the
// point is to prevent huge amounts of fragmentation when a long
// string is appended to a short one.  The question, of course, is
// whether more appends are coming, in which case we should give it
// more capacity.  It might be better to always grow to a multiple of
// the current capacity when the capacity is exceeded.

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer3:[x0-9a-f]+]][0...48], capacity = 48))
// CHECK-NOT: .Native@[[buffer2]]
s += s + s
println("\(repr(s))")

// -- expect a reallocation here

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer4:[x0-9a-f]+]][0...49], capacity = 96))
// CHECK-NOT: .Native@[[buffer3]]
s += "C"
println("\(repr(s))")

/// An additional reference to the same buffer doesn't, by itself,
/// impede the use of available capacity
var s1 = s

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer4]][0...50], capacity = 96))
s += "F"
println("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer4]][0...49], capacity = 96))
println("\(repr(s1))")

/// The use of later buffer capacity by another string forces
/// reallocation

// CHECK-NEXT: String{{.*}} = {{.*}}X"
// CHECK-NOT: .Native@[[buffer4]]
s1 += "X"
println("\(repr(s1))")

/// Appending to an empty string re-uses the RHS

// CHECK-NEXT: .Native@[[buffer4]]
var s2 = String()
s2 += s
println("\(repr(s2))")
