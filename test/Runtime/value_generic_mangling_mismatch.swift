// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// A mangled type name may place an integer (value) generic argument where a
// type is expected, e.g. `_typeByName("$99_Sg")` asks for `Optional<99>`. The
// runtime represents metadata and values the same way, so without validation
// the integer is misinterpreted as a type metadata pointer and dereferenced,
// crashing. Such names must instead fail the lookup gracefully, while genuine
// value generics (an integer in a value parameter) keep working.

import Swift
import StdlibUnittest

struct ValG<let N: Int> {}
struct OuterG<T> { struct ValInner<let M: Int> {} }
struct PairG<T, U> {}
protocol P {}
struct S: P {}

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

runAllTests()
