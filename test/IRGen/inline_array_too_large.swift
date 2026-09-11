// A fixed-layout type's storage size (and a class's instance size) is recorded
// in a 32-bit field, both in the compiler and in the runtime metadata. A type
// whose size does not fit in 32 bits must be rejected instead of silently
// truncating the size, which would otherwise produce a miscompile in which
// layouts and allocations disagree (a linear heap overflow for classes).
//
// Each variant is compiled in isolation so that an earlier error does not stop
// IR generation before the later type is laid out.

// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DARRAY  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=ARRAY
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DSTRUCT -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=STRUCT
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DCLASS  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=CLASS
// RUN: not %target-swift-frontend -disable-availability-checking -primary-file %s -DTUPLE  -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s --check-prefix=TUPLE

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
// Two fields, each under UINT32_MAX (300000000 * 8 = 2400000000), that sum to
// more than UINT32_MAX.
struct Big {
  var a: InlineArray<300000000, Int64>
  var b: InlineArray<300000000, Int64>
}
func use() -> Int { MemoryLayout<Big>.size }
// STRUCT: error: type 'Big' is too large to be represented

#elseif CLASS
// A class whose instance size exceeds the 32-bit InstanceSize metadata field.
class C {
  var a = InlineArray<300000000, Int64>(repeating: 0)
  var b = InlineArray<300000000, Int64>(repeating: 0)
}
func use() -> C { C() }
// CLASS: error: type 'C' is too large to be represented

#elseif TUPLE
typealias T = (InlineArray<300000000, Int64>, InlineArray<300000000, Int64>)
func use() -> Int { MemoryLayout<T>.size }
// TUPLE: error: type is too large to be represented

#endif
