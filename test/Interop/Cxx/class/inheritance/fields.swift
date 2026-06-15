// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Fields

var FieldsTestSuite = TestSuite("Getting and setting fields in base classes")

struct SwiftStructWrapper {
  let value: DerivedFromOneField
}

func optionalDerivedFromAll() -> DerivedFromAll? { DerivedFromAll() }

FieldsTestSuite.test("Fields from derived from all") {
  let derived = DerivedFromAll()
  expectEqual(derived.a, 1)
  expectEqual(derived.b, 2)
  expectEqual(derived.c, 3)
  expectEqual(derived.d, 4)
  expectEqual(derived.e, 5)
  expectEqual(derived.f, 6)

  var mutable = DerivedFromAll()
  mutable.a = 42
  mutable.d = 44
  mutable.e = 46
  mutable.f = 48

  expectEqual(mutable.a, 42)
  expectEqual(mutable.d, 44)
  expectEqual(mutable.e, 46)
  expectEqual(mutable.f, 48)
}

FieldsTestSuite.test("Optional") {
  let derived = optionalDerivedFromAll()
  expectEqual(derived!.a, 1)
  expectEqual(derived!.b, 2)
  expectEqual(derived!.c, 3)
  expectEqual(derived!.d, 4)
  expectEqual(derived!.e, 5)
  expectEqual(derived!.f, 6)
}

FieldsTestSuite.test("Struct holding derived from one field") {
  let derived = DerivedFromOneField()
  let s = SwiftStructWrapper(value: derived)

  expectEqual(s.value.value, 42)
}

FieldsTestSuite.test("Fields from derived from non trivial") {
  let derived = NonTrivialDerivedFromAll()
  expectEqual(derived.a, 1)
  expectEqual(derived.b, 2)
  expectEqual(derived.c, 3)
  expectEqual(derived.d, 4)
  expectEqual(derived.e, 5)
  expectEqual(derived.f, 6)

  var mutable = NonTrivialDerivedFromAll()
  mutable.a = 42
  mutable.d = 44
  mutable.e = 46
  mutable.f = 48

  expectEqual(mutable.a, 42)
  expectEqual(mutable.d, 44)
  expectEqual(mutable.e, 46)
  expectEqual(mutable.f, 48)
}

FieldsTestSuite.test("Derived from class template") {
  var derived = DerivedFromClassTemplate()
  derived.value = 42
  expectEqual(derived.value, 42)
}

FieldsTestSuite.test("Same field from derived") {
  var derived = DerivedWithSameField()
  derived.a = 42
  expectEqual(derived.a, 42)
}

extension CopyTrackedBaseClass {
    var swiftProp: CInt {
        return x
    }
}

FieldsTestSuite.test("Get field without copying base in the getter accessor") {
  let base = CopyTrackedBaseClass(0)
  var copyCounter = getCopyCounter().pointee
  expectEqual(base.swiftProp, 0)
  // Measure copy counter of a regular
  // property access for a C++ type to compare
  // it to see if any additional copies are
  // needed to access the property from the base class.
  let expectedCopyCountDiff = getCopyCounter().pointee - copyCounter

  let derived = CopyTrackedDerivedClass(234)
  copyCounter = getCopyCounter().pointee
  expectEqual(derived.x, 234)
  expectEqual(copyCounter, getCopyCounter().pointee - expectedCopyCountDiff)

  let derivedDerived = CopyTrackedDerivedDerivedClass(-11)
  copyCounter = getCopyCounter().pointee
  expectEqual(derivedDerived.x, -11)
  expectEqual(copyCounter, getCopyCounter().pointee - expectedCopyCountDiff)
}

FieldsTestSuite.test("Structs with virtual methods") {
  var derived = InheritFromStructsWithVirtualMethod()
  derived.d = 42
  expectEqual(derived.d, 42)
}

FieldsTestSuite.test("Field in tail padding of base class") {
  let usesBaseTailPadding = DerivedUsesBaseTailPadding()
  expectEqual(usesBaseTailPadding.field2, 789)
  expectEqual(usesBaseTailPadding.field4, 456)
  expectEqual(usesBaseTailPadding.field8, 123)
}

FieldsTestSuite.test("Out-of-order inheritance") {
  let d = DerivedOutOfOrder()
  expectEqual(d.leafField, 789)
  expectEqual(d.derivedField, 456)
  expectEqual(d.baseField, 123)
}

FieldsTestSuite.test("Empty subobjects") {
  _ = makeBaseEmpty()
  _ = makeEmptyOnlyDerived()
}

FieldsTestSuite.test("Contain empty fields") {
  let o1 = makeIntAndEmpty()
  expectEqual(o1.x, 42)

  let o2 = makeNoUniqueAddressEmpty()
  expectEqual(o2.x, 17)
}

FieldsTestSuite.test("Multiple empty bases") {
  _ = makeMultiEmptyBases()
  _ = makeMultiBaseWithEmpty()
}

FieldsTestSuite.test("Empty as nested or array field") {
  _ = makeContainsEmptyClass()

  let o = makeOuterWithEmpty()
  expectEqual(o.x, 7)

  _ = makeEmptyArrayHolder()
}

FieldsTestSuite.test("Empty as template type parameter") {
  let o1 = makeEmptyHolderOfEmpty()
  expectEqual(o1.x, false)

  let o2 = makeEmptyHolderOfInt()
  expectEqual(o2.x, false)
  expectEqual(o2.t, 2)
}

FieldsTestSuite.test("Empty virtual classes") {
  _ = makeHasFieldWithOnlyVDtor()
  _ = makeWrapsVirtualBase()
}

FieldsTestSuite.test("Empty field + 2 chars") {
  let c1 = makeChildEmptyAndTwoChars()
  expectEqual(c1.a, CChar(65))  // 'A'
  expectEqual(c1.b, CChar(66))  // 'B'
}

FieldsTestSuite.test("Empty field + int + char") {
  _ = makeChildEmptyIntChar()
}

FieldsTestSuite.test("Recursively empty field") {
  let _ = S3()
}

FieldsTestSuite.test("Empty struct as parameter") {
  expectEqual(takeBaseEmpty(BaseEmpty(), 17), 17)
  expectEqual(takeBaseEmpty(makeBaseEmpty(), 71), 71)

  let b1 = roundTripBaseEmpty(makeBaseEmpty())
  _ = b1

  let d = roundTripEmptyOnlyDerived(makeEmptyOnlyDerived())
  _ = d
}

FieldsTestSuite.test("Hollow type stored in Swift containers") {
  let arr = [makeBaseEmpty(), makeBaseEmpty(), makeBaseEmpty()]
  expectEqual(arr.count, 3)

  let tup = (makeBaseEmpty(), 42, makeEmptyOnlyDerived())
  expectEqual(tup.1, 42)

  struct SwiftWrapper {
    var inner: BaseEmpty
    var n: Int
  }
  let w = SwiftWrapper(inner: makeBaseEmpty(), n: 5)
  expectEqual(w.n, 5)
}

FieldsTestSuite.test("Unions") {
  let s1 = makeHasEmptyUnion()
  expectEqual(s1.i, 3)
  let s2 = makeHasUnionEmptyAndInt()
  expectEqual(s2.i, 5)
  let s3 = makeHasUnionEmptyAndIntWithValue()
  expectEqual(s3.i, 5)
  expectEqual(s3.u.x, 15)
}

runAllTests()
