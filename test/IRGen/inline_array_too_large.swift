// A fixed-layout type's storage size (and a class's instance size) is recorded
// in a 32-bit field, both in the compiler and in the runtime metadata. A type
// whose size does not fit in 32 bits must be rejected instead of silently
// truncating the size, which would otherwise produce a miscompile in which
// layouts and allocations disagree (a linear heap overflow for classes).
//
// Each variant is compiled in isolation so that an earlier error does not stop
// IR generation before the later type is laid out.

// UNSUPPORTED: CPU=wasm32

// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DARRAY  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=ARRAY
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DSTRUCT -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=STRUCT
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DCLASS  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CLASS
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DCLASS_FIELD_OFFSET -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CLASS_FIELD_OFFSET
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DSUBCLASS -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=SUBCLASS
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DTUPLE  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=TUPLE
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DENUM   -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=ENUM
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DENUM_TAG -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=ENUM_TAG
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DENUM_MULTI -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=ENUM_MULTI

#if ARRAY
// A single fixed array whose byte size exceeds 32 bits:
// 536870913 * 8 = 4294967304 > UINT32_MAX.
struct S {
  var a: InlineArray<536870913, Int64>
  var b: Int64
}
func use() -> Int { MemoryLayout<S>.size }
// ARRAY: error: type 'Builtin.FixedArray<536870913, Int64>' is too large to be represented

#elseif STRUCT
// Two fields, each under 512 MiB (60000000 * 8 = 480000000), that sum to
// more than 512 MiB.
struct Big {
  var a: InlineArray<60000000, Int64>
  var b: InlineArray<60000000, Int64>
}
func use() -> Int { MemoryLayout<Big>.size }
// STRUCT: error: type 'Big' is too large to be represented

#elseif CLASS
// A class whose instance size exceeds the 32-bit InstanceSize metadata field.
class C {
  var a = InlineArray<60000000, Int64>(repeating: 0)
  var b = InlineArray<60000000, Int64>(repeating: 0)
  var c = InlineArray<60000000, Int64>(repeating: 0)
  var d = InlineArray<60000000, Int64>(repeating: 0)
  var e = InlineArray<60000000, Int64>(repeating: 0)
  var f = InlineArray<60000000, Int64>(repeating: 0)
  var g = InlineArray<60000000, Int64>(repeating: 0)
  var h = InlineArray<60000000, Int64>(repeating: 0)
  var i = InlineArray<60000000, Int64>(repeating: 0)
}
func use() -> C { C() }
// CLASS: error: type 'C' is too large to be represented

#elseif CLASS_FIELD_OFFSET
// A class whose last field starts past UINT32_MAX, so its field offset does not
// fit in 32 bits either.
class C3 {
  var a = InlineArray<60000000, Int64>(repeating: 0)
  var b = InlineArray<60000000, Int64>(repeating: 0)
  var c = InlineArray<60000000, Int64>(repeating: 0)
  var d = InlineArray<60000000, Int64>(repeating: 0)
  var e = InlineArray<60000000, Int64>(repeating: 0)
  var f = InlineArray<60000000, Int64>(repeating: 0)
  var g = InlineArray<60000000, Int64>(repeating: 0)
  var h = InlineArray<60000000, Int64>(repeating: 0)
  var i = InlineArray<60000000, Int64>(repeating: 0)
  var j = InlineArray<60000000, Int64>(repeating: 0)
}
func use() -> C3 { C3() }
// CLASS_FIELD_OFFSET: error: type 'C3' is too large to be represented

#elseif SUBCLASS
// The superclass fits; the subclass's own fields push the instance size past
// UINT32_MAX.
class Base {
  var a = InlineArray<60000000, Int64>(repeating: 0)
}
class Derived: Base {
  var b = InlineArray<60000000, Int64>(repeating: 0)
  var c = InlineArray<60000000, Int64>(repeating: 0)
  var d = InlineArray<60000000, Int64>(repeating: 0)
  var e = InlineArray<60000000, Int64>(repeating: 0)
  var f = InlineArray<60000000, Int64>(repeating: 0)
  var g = InlineArray<60000000, Int64>(repeating: 0)
  var h = InlineArray<60000000, Int64>(repeating: 0)
  var i = InlineArray<60000000, Int64>(repeating: 0)
}
func use() -> Derived { Derived() }
// SUBCLASS: error: type 'Derived' is too large to be represented

#elseif TUPLE
typealias T = (InlineArray<60000000, Int64>, InlineArray<60000000, Int64>)
func use() -> Int { MemoryLayout<T>.size }
// TUPLE: error: type is too large to be represented

#elseif ENUM
// The radar's enum, whose payload is exactly 512 MiB.
enum E {
  case a(InlineArray<536870912, UInt8>)
  case b
}
func use() -> Int { MemoryLayout<E>.size }
// ENUM: error: type 'Builtin.FixedArray<536870912, UInt8>' is too large to be represented

#elseif ENUM_TAG
// The payload fits, but the tag byte makes the enum 512 MiB.
enum E {
  case a(InlineArray<536870911, UInt8>)
  case b
}
func use() -> Int { MemoryLayout<E>.size }
// ENUM_TAG: error: type 'E' is too large to be represented

#elseif ENUM_MULTI
// A multi-payload enum whose payload fits, but whose tag byte makes it
// 512 MiB.
enum E {
  case a(InlineArray<536870911, UInt8>)
  case b(InlineArray<536870911, UInt8>)
  case c
}
// ENUM_MULTI: error: type 'E' is too large to be represented

#endif
