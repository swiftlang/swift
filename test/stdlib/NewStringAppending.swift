// RUN: %target-run-stdlib-swift | FileCheck %s
//
// Parts of this test depend on memory allocator specifics.  The test
// should be rewritten soon so it doesn't expose legacy components
// like OpaqueString anyway, so we can just disable the failing
// configuration
// XFAIL: asan
//
// Memory allocator specifics also vary across platorms.
// REQUIRES: ARCH=x86_64, OS=macosx

import Foundation
import Swift

func hexAddr(x: AnyObject) -> String {
  return hexAddr(Builtin.bridgeToRawPointer(Builtin.castToObjectPointer(x)))
}

func hexAddr(x: Builtin.RawPointer) -> String {
  return "@0x" + Int(Builtin.ptrtoint_Word(x)).format('x', "")
}

func hexAddr<T>(p: UnsafePointer<T>) -> String {
  return hexAddr(p.value)
}

func hexAddr(x: ContiguousString.Owner) -> String {
  switch x {
  case .None:
    return "null"
  case .Native(var y): 
    var z = UnsafePointer<Builtin.RawPointer>(Builtin.addressof(&y))
    return ".Native\(hexAddr(z.get()))"
  case .Cocoa(var y): 
    return ".Cocoa\(hexAddr(y))"
  }
}

func repr(x: NSString) -> String {
  return "\(NSStringFromClass(object_getClass(x)))\(hexAddr(x)) = \"\(x)\""
}

func repr(x: OpaqueString) -> String {
  var x2 = x
  return "Opaque("
         + "buffer: \(hexAddr(x2.buffer))"
         + "[\(x.range.startIndex)...\(x.range.endIndex)])"
}

func repr(x: ContiguousString) -> String {
  switch x.owner {
  case .Native(var bb):
    var b = StringBuffer(bb)
    var offset = x.isUTF16
      ? UnsafePointer(b.start) - x.startUTF16
      : UnsafePointer(b.start) - x.startASCII

    return "Contiguous(owner: \(hexAddr(x.owner))[\(offset)...\(x.count + offset)], capacity = \(b.capacity))"
  default:  
    return "Contiguous(owner: \(hexAddr(x.owner)), count: \(x.count))"
  }
}

func repr(x: String) -> String {
  switch x.representation {
  case .Contiguous(var rep):
    return "String(\(repr(rep))) = \"\(x)\""
  case .Opaque(var rep):
    return "String(\(repr(rep))) = \"\(x)\""
  }
}


// ===------- Appending -------===

// CHECK: --- Appending ---
println("--- Appending ---")

var s = "⓪" // start non-empty

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer1:[x0-9a-f]+]][0...2], capacity = 8)) = "⓪1"
s += '1'
println("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer1]][0...8], capacity = 8)) = "⓪1234567"
s += "234567"
println("\(repr(s))")

// -- expect a reallocation here

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer2:[x0-9a-f]+]][0...9], capacity = 16)) = "⓪12345678"
// CHECK-NOT: .Native@[[buffer1]]
s += '8'
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
s += 'C'
println("\(repr(s))")

/// An additional reference to the same buffer doesn't, by itself,
/// impede the use of available capacity
var s1 = s

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer4]][0...50], capacity = 96))
s += 'F'
println("\(repr(s))")

// CHECK-NEXT: String(Contiguous(owner: .Native@[[buffer4]][0...49], capacity = 96))
println("\(repr(s1))")

/// The use of later buffer capacity by another string forces
/// reallocation

// CHECK-NEXT: String{{.*}} = {{.*}}X"
// CHECK-NOT: .Native@[[buffer4]]
s1 += 'X'
println("\(repr(s1))")

/// Appending to an empty string re-uses the RHS

// CHECK-NEXT: .Native@[[buffer4]]
var s2 = String()
s2 += s
println("\(repr(s2))")
