// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -enable-experimental-feature AddressableParameters)
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -cxx-interoperability-mode=default -Xcc -std=c++20)
//
// REQUIRES: executable_test
// REQUIRES: swift_feature_AddressableParameters

// Undefined hidden symbol to C++ voidify in libcxx
// rdar://121551667
// XFAIL: OS=freebsd

import StdlibUnittest
import StdOptional
import CxxStdlib

var StdOptionalTestSuite = TestSuite("StdOptional")

StdOptionalTestSuite.test("pointee") {
  let nonNilOpt = getNonNilOptional()
  let pointee = nonNilOpt.pointee
  expectEqual(123, pointee)

  let nonNilOptNonCopyable = getNonNilOptionalHasDeletedCopyCtor()
  expectEqual(654, nonNilOptNonCopyable.pointee.value)
}

StdOptionalTestSuite.test("std::optional => Swift.Optional") {
  let nonNilOpt = getNonNilOptional()
  let swiftOptional = Optional(fromCxx: nonNilOpt)
  expectNotNil(swiftOptional)
  expectEqual(123, swiftOptional!)

  let nilOpt = getNilOptional()
  let swiftNil = Optional(fromCxx: nilOpt)
  expectNil(swiftNil)
}

StdOptionalTestSuite.test("std::optional hasValue/value") {
  let nonNilOpt = getNonNilOptional()
  expectTrue(nonNilOpt.hasValue)
  expectEqual(123, nonNilOpt.value!)

  let nilOpt = getNilOptional()
  expectFalse(nilOpt.hasValue)
  expectNil(nilOpt.value)
}

StdOptionalTestSuite.test("std::optional value setter") {
  var nonNilOpt = getNonNilOptional()
  nonNilOpt.value = 321
  expectEqual(321, nonNilOpt.value!)
  nonNilOpt.value = 456
  expectEqual(456, nonNilOpt.value!)
  nonNilOpt.value = nil
  expectNil(nonNilOpt.value)

  var nilOpt = getNilOptional()
  nilOpt.value = 321
  expectEqual(321, nilOpt.value!)
}

StdOptionalTestSuite.test("std::optional as ExpressibleByNilLiteral") {
  let res1 = takesOptionalInt(nil)
  expectFalse(res1)

  let res2 = takesOptionalString(nil)
  expectFalse(res2)
}

StdOptionalTestSuite.test("std::optional init(_:Wrapped)") {
  let optInt = StdOptionalInt(123)
  expectEqual(123, optInt.pointee)

  // FIXME: making these variables immutable triggers a miscompile on Linux
  // (https://github.com/swiftlang/swift/issues/82765)
  var optBoolTMutable = StdOptionalBool(true)
  var optBoolFMutable = StdOptionalBool(false)
  expectTrue(optBoolTMutable.pointee)
  expectFalse(optBoolFMutable.pointee)

  // If AddressableParameters are enabled, this issue does not happen.
#if hasFeature(AddressableParameters)
  let optBoolT = StdOptionalBool(true)
  let optBoolF = StdOptionalBool(false)
  expectTrue(optBoolT.pointee)
  expectFalse(optBoolF.pointee)
#endif

  let optString = StdOptionalString(std.string("abc"))
  expectEqual(std.string("abc"), optString.pointee)

  let optOptInt = StdOptionalOptionalInt(StdOptionalInt(456))
  expectEqual(456, optOptInt.pointee.pointee)

  let optConstexprCtor = StdOptionalHasConstexprCtor(HasConstexprCtor(321))
  expectEqual(321, optConstexprCtor.pointee.value)
}

StdOptionalTestSuite.test("std::optional of move-only std::vector") {
  let optVectorNonCopyable = getNonNilOptionalMoveOnlyVector()
  expectEqual(2, optVectorNonCopyable.pointee.size())
  expectEqual(789, optVectorNonCopyable.pointee[0].value)
  expectEqual(987, optVectorNonCopyable.pointee[1].value)
}

func hasValueGeneric<O: CxxOptional & ~Copyable>(_ opt: borrowing O) -> Bool
where O.Wrapped: ~Copyable {
  return opt.hasValue
}

func pointeeValueGeneric<O: CxxOptional<MoveOnlyCounted> & ~Copyable>(
  _ opt: borrowing O
) -> Int32 {
  return opt.pointee.value
}

func makeGeneric<O: CxxOptional<MoveOnlyCounted> & ~Copyable>(
  _: O.Type, _ value: consuming MoveOnlyCounted
) -> O {
  return O(value)
}

func makeNilGeneric<O: CxxOptional & ~Copyable>(_: O.Type) -> O
where O.Wrapped: ~Copyable {
  return nil
}

StdOptionalTestSuite.test("std::optional of move-only type") {
  do {
    let opt = getNonNilOptionalHasDeletedCopyCtor()
    expectTrue(opt.hasValue)
    expectEqual(654, opt.pointee.value)

    let opt2 = StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(321))
    expectTrue(opt2.hasValue)
    expectEqual(321, opt2.pointee.value)

    let nilOpt: StdOptionalHasDeletedCopyCtor = nil
    expectFalse(nilOpt.hasValue)

    expectFalse(takesOptionalHasDeletedCopyCtor(nil))
    expectTrue(takesOptionalHasDeletedCopyCtor(
      StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(1))))
  }

  do {
    let opt = StdOptionalMoveOnlyVector(makeMoveOnlyVector())
    expectTrue(opt.hasValue)
    expectEqual(2, opt.pointee.size())
    expectEqual(111, opt.pointee[0].value)
    expectEqual(222, opt.pointee[1].value)
  }

  do {
    let opt = StdOptionalAnnotatedNonCopyable(AnnotatedNonCopyable(5))
    expectTrue(hasValueGeneric(opt))
    expectEqual(5, opt.pointee.value)
  }

  do {
    let opt = StdOptionalOptionalHasDeletedCopyCtor(
      StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(77)))
    expectTrue(opt.hasValue)
    expectTrue(opt.pointee.hasValue)
    expectEqual(77, opt.pointee.pointee.value)

    let innerNil: StdOptionalHasDeletedCopyCtor = nil
    let outerOfNil = StdOptionalOptionalHasDeletedCopyCtor(innerNil)
    expectTrue(outerOfNil.hasValue)
    expectFalse(outerOfNil.pointee.hasValue)
  }

  do {
    let opt = getNonNilOptionalMoveOnlyCounted()
    expectEqual(1, getMoveOnlyCountedAlive())
    expectTrue(hasValueGeneric(opt))
    expectEqual(42, pointeeValueGeneric(opt))
  }
  expectEqual(0, getMoveOnlyCountedAlive())

  do {
    var opt = StdOptionalMoveOnlyCounted(MoveOnlyCounted(7))
    expectEqual(1, getMoveOnlyCountedAlive())
    expectEqual(7, opt.pointee.value)
    opt = nil
    expectEqual(0, getMoveOnlyCountedAlive())
    expectFalse(opt.hasValue)
  }
  expectEqual(0, getMoveOnlyCountedAlive())

  do {
    let opt = makeGeneric(StdOptionalMoveOnlyCounted.self, MoveOnlyCounted(9))
    expectEqual(1, getMoveOnlyCountedAlive())
    expectTrue(hasValueGeneric(opt))
    expectEqual(9, pointeeValueGeneric(opt))

    let nilOpt = makeNilGeneric(StdOptionalMoveOnlyCounted.self)
    expectFalse(hasValueGeneric(nilOpt))
  }
  expectEqual(0, getMoveOnlyCountedAlive())
}

runAllTests()
