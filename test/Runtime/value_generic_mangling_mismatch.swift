// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Ensure that a mangled type name binding a generic argument of one sort to a
// generic parameter of the other, in either direction, fails the lookup
// gracefully rather than confusing a metadata pointer with an integer value,
// and that genuine value generics keep resolving. A mangled `$N_` is the value
// N + 1, and rejected integers come in pairs: an even value is
// indistinguishable from a metadata pointer, while an odd one is caught by the
// runtime's metadata/pack discriminator.

import Swift
import StdlibUnittest

#if _runtime(_ObjC)
import Foundation
#endif

struct ValG<let N: Int> {}
struct OuterG<T> { struct ValInner<let M: Int> {} }
struct PairG<T, U> {}
struct Holder<let N: Int, T> { var storage: InlineArray<N, T>? = nil }
struct A<T> { struct B<let M: Int> { struct C<U> { struct D<let K: Int> {} } } }
struct ValOuter<let N: Int> { struct TypeInner<T> {} }
protocol P {}
struct S: P {}

#if _runtime(_ObjC)
class ValClass<T, let N: Int> {}
#endif

let tests = TestSuite("ValueGenericManglingMismatch")

tests.test("integer in a value parameter resolves") {
  // Top-level value parameter.
  expectEqual(ValG<3>.self, _typeByName("4main4ValGVy$2_G")!)
  // Value parameter nested under a type parameter at an outer level.
  expectEqual(OuterG<Int>.ValInner<5>.self,
              _typeByName("4main6OuterGV8ValInnerVySi_$4_G")!)
  // Plain type parameters keep working too.
  expectEqual(PairG<Int, String>.self, _typeByName("4main5PairGVySiSSG")!)
  // The standard-library value generic.
  expectEqual(InlineArray<4, Int>.self, _typeByName("s11InlineArrayVy$3_SiG")!)
  // A protocol-scoped concrete base type resolves to the base.
  expectEqual(S.self, _typeByName("4main1PPy4main1SVG")!)
}

tests.test("integer bound to a type parameter fails gracefully") {
  // `Optional<99>` and `Array<99>`: integer where Wrapped/Element go.
  expectNil(_typeByName("$99_Sg"))
  expectNil(_typeByName("Say$99_G"))
  // An unparseable integer in Optional.
  expectNil(_typeByName("$Sg"))
  // Integer in either type parameter of `PairG<T, U>`.
  expectNil(_typeByName("4main5PairGVy$98_SSG"))
  expectNil(_typeByName("4main5PairGVySi$98_G"))
  // Multi-level: an integer wrongly bound to `OuterG`'s type parameter while a
  // *legitimate* value sits in the inner `ValInner.M`. Must still be rejected
  // at the outer level rather than confused by the valid inner value.
  expectNil(_typeByName("4main6OuterGV8ValInnerVy$0__$4_G"))
}

tests.test("integer bound to a protocol generic parameter fails gracefully") {
  expectNil(_typeByName("4main1PPy$99_G"))
}

tests.test("integer at the root of a type name fails gracefully") {
  // A bare value generic where a type is expected. An even value (256) would
  // otherwise be indistinguishable from a metadata pointer and dereferenced;
  // an odd value (255) is caught by the runtime's metadata/pack discriminator.
  // Type lookup rejects a root value (a separate value-lookup entry point
  // resolves these legitimately); both must fail _typeByName gracefully.
  expectNil(_typeByName("$255_"))
  expectNil(_typeByName("$254_"))
}

tests.test("integer where a tuple element type is expected fails gracefully") {
  // `(100, Int)`: an integer sits where the first element type belongs. An
  // even value passes the runtime's metadata/pack discriminator and would be
  // dereferenced as a type metadata pointer.
  expectNil(_typeByName("$99__Sit"))
  // An odd value (`(99, Int)`) currently happens to look like a metadata pack
  // and is rejected, but it must keep failing gracefully too.
  expectNil(_typeByName("$98__Sit"))
}

tests.test("integer where a function parameter type is expected fails gracefully") {
  // `(100) -> Int`: integer in the parameter position.
  expectNil(_typeByName("Si$99_c"))
  expectNil(_typeByName("Si$98_c"))
  // `(100, String) -> Bool`: integer as a parameter inside the parameter tuple.
  expectNil(_typeByName("Sb$99__SStc"))
}

tests.test("integer where a function result type is expected fails gracefully") {
  // `(Int) -> 100`: integer in the result position.
  expectNil(_typeByName("$99_Sic"))
  expectNil(_typeByName("$98_Sic"))
}

tests.test("integer where a metatype instance type is expected fails gracefully") {
  // `100.Type`: integer as the instance of a metatype.
  expectNil(_typeByName("$99_m"))
  expectNil(_typeByName("$98_m"))
}

tests.test("integer where a Builtin.FixedArray element type is expected fails gracefully") {
  // `Builtin.FixedArray<3, 100>`: the count is legitimately a value, but the
  // element must be a type. createBuiltinFixedArrayType calls getMetadata() on
  // the element with no guard, so an even value is dereferenced and an odd
  // value aborts in getMetadata() rather than failing the lookup.
  expectNil(_typeByName("$2_$99_BV"))
  expectNil(_typeByName("$2_$98_BV"))
}

tests.test("type where a Builtin.FixedArray count is expected fails gracefully") {
  expectNil(_typeByName("SiSiBV"))
  expectNil(_typeByName("SdSiBV"))
  expectNil(_typeByName("$2_SiBVSiBV"))
}

tests.test("type bound to a value generic parameter fails gracefully") {
  expectNil(_typeByName("s11InlineArrayVySiSiG"))
  expectNil(_typeByName("s11InlineArrayVySSSiG"))
  expectNil(_typeByName("4main4ValGVySiG"))
  expectNil(_typeByName("4main6OuterGV8ValInnerVySi_SiG"))
  expectNil(_typeByName("4main1AV1BVySi_SiG"))
  expectNil(_typeByName("4main1AV1BV1CV1DVySi_$2__SS_SiG"))
  expectNil(_typeByName("4main1AV1BVy$2__$2_G"))
  expectNil(_typeByName("4main1AV1BV1CV1DVySi_$2__$9__$6_G"))
}

// A mangled name may bind either just the innermost type's own parameters, as
// above, or the complete set across every level of nesting in one argument
// list. The two shapes index different parameter lists.
tests.test("wrong argument sort in a whole-nesting argument list fails gracefully") {
  expectNil(_typeByName("4main8ValOuterV9TypeInnerVySiSSG"))
  expectNil(_typeByName("4main1AV1BVySiSiG"))
  expectNil(_typeByName("4main1AV1BV1CV1DVySi$2_SSSiG"))
  expectNil(_typeByName("4main1AV1BVy$2_$2_G"))
  expectNil(_typeByName("4main1AV1BV1CV1DVySi$2_$9_$6_G"))
}

tests.test("unresolved Builtin.FixedArray element type fails gracefully") {
  expectNil(_typeByName("$3_xBV"))
  expectNil(_typeByName("$3_qd__BV"))
  expectNil(_typeByName("SixBV"))
}

tests.test("pack in a Builtin.FixedArray position fails gracefully") {
  expectNil(_typeByName("$2_Si_SSQPBV"))
  expectNil(_typeByName("Si_SSQPSiBV"))
}

tests.test("genuine Builtin.FixedArray manglings still resolve") {
  func sizeOf<T>(_: T.Type) -> Int { MemoryLayout<T>.size }
  if let t = expectNotNil(_typeByName("$3_SiBV")) {
    expectEqual(4 * MemoryLayout<Int>.stride, _openExistential(t, do: sizeOf))
  }
  // A negative count and an unresolved dependent count both lay out like the
  // empty tuple.
  if let t = expectNotNil(_typeByName("$n0_SiBV")) {
    expectEqual(0, _openExistential(t, do: sizeOf))
  }
  if let t = expectNotNil(_typeByName("xSiBV")) {
    expectEqual(0, _openExistential(t, do: sizeOf))
  }
}

tests.test("genuine value generic manglings still resolve") {
  expectEqual(InlineArray<4, String>.self,
              _typeByName("s11InlineArrayVy$3_SSG")!)
  expectEqual(InlineArray<0, Int>.self, _typeByName("s11InlineArrayVy$_SiG")!)
  expectEqual(InlineArray<2, InlineArray<3, Int>>.self,
              _typeByName("s11InlineArrayVy$1_ABy$2_SiGG")!)
  expectEqual([InlineArray<3, Double>].self,
              _typeByName("Says11InlineArrayVy$2_SdGG")!)
  expectEqual(Optional<InlineArray<7, UInt8>>.self,
              _typeByName("s11InlineArrayVy$6_s5UInt8VGSg")!)
  expectEqual(Holder<3, Int>.self, _typeByName("4main6HolderVy$2_SiG")!)
  expectEqual(Holder<9, InlineArray<2, Int>>.self,
              _typeByName("4main6HolderVy$8_s11InlineArrayVy$1_SiGG")!)
  expectEqual(A<Int>.B<3>.self, _typeByName("4main1AV1BVySi_$2_G")!)
  expectEqual(A<Int>.B<3>.C<String>.D<7>.self,
              _typeByName("4main1AV1BV1CV1DVySi_$2__SS_$6_G")!)
}

tests.test("genuine value generics in a whole-nesting argument list still resolve") {
  expectEqual(ValOuter<3>.TypeInner<String>.self,
              _typeByName("4main8ValOuterV9TypeInnerVy$2_SSG")!)
  expectEqual(A<Int>.B<3>.self, _typeByName("4main1AV1BVySi$2_G")!)
  expectEqual(A<Int>.B<3>.C<String>.D<7>.self,
              _typeByName("4main1AV1BV1CV1DVySi$2_SS$6_G")!)
}

// The runtime mangles metadata using a symbolic reference to the innermost
// nominal type, which produces the whole-nesting shape.
tests.test("metadata for a nested value generic round-trips through its mangled name") {
  expectEqual("main.ValOuter<3>.TypeInner<Swift.String>",
              _typeName(ValOuter<3>.TypeInner<String>.self))
  expectEqual(ValOuter<3>.TypeInner<String>.self,
              _typeByName(_mangledTypeName(ValOuter<3>.TypeInner<String>.self)!)!)
  expectEqual(A<Int>.B<3>.C<String>.D<7>.self,
              _typeByName(_mangledTypeName(A<Int>.B<3>.C<String>.D<7>.self)!)!)
}

#if _runtime(_ObjC)
// objc_getClass routes an unprefixed mangled name through the same lookup.
tests.test("type bound to a value generic parameter fails gracefully through objc_getClass") {
  expectNotNil(NSClassFromString(_mangledTypeName(ValClass<Int, 5>.self)!))
  expectNil(NSClassFromString("4main8ValClassCySiSiG"))
}
#endif

runAllTests()
