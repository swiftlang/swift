// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -Xfrontend -disable-availability-checking -Onone)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import StdlibUnittest
import Upcast

var UpcastTestSuite = TestSuite("Foreign Reference Type Upcast")

UpcastTestSuite.test("Derived as Base") {
  let d = Derived.create()
  let b: Base = d as Base
  expectEqual(b.getBaseValue(), 1)
  expectEqual(d.getDerivedValue(), 2)
}

UpcastTestSuite.test("LeafDerived as Base") {
  let l = LeafDerived.create()
  let lb: Base = l as Base
  expectEqual(lb.getBaseValue(), 1)
}

UpcastTestSuite.test("LeafDerived as Derived") {
  let l = LeafDerived.create()
  let ld: Derived = l as Derived
  expectEqual(ld.getDerivedValue(), 2)
}

UpcastTestSuite.test("implicit conversion in function call") {
  let d = Derived.create()
  let l = LeafDerived.create()
  func takesBase(_ b: Base) -> Int32 { b.getBaseValue() }
  expectEqual(takesBase(d), 1)
  expectEqual(takesBase(l), 1)
}

UpcastTestSuite.test("implicit conversion in return position") {
  func makeBase() -> Base {
    let d = Derived.create()
    d.setBaseValue(77)
    return d
  }
  let b = makeBase()
  expectEqual(b.getBaseValue(), 77)

  b.setBaseValue(1)
}

UpcastTestSuite.test("implicit conversion in return position refcounted") {
  func makeRefBase() -> RefCountedBase {
    return RefCountedDerived.create()
  }
  let b = makeRefBase()
  expectEqual(b.getBaseValue(), 10)

  expectTrue(b.refCount > 0)
  expectTrue(b.refCount < 10)
}

UpcastTestSuite.test("RefCountedDerived as RefCountedBase") {
  let d = RefCountedDerived.create()
  expectTrue(d is RefCountedBase)

  let b = d as RefCountedBase
  expectEqual(b.getBaseValue(), 10)
  expectEqual(d.getDerivedField(), 20)

  expectTrue(d.refCount > 0)
  expectTrue(d.refCount < 10)
}

UpcastTestSuite.test("refcounted implicit conversion") {
  func takesRefBase(_ b: RefCountedBase) -> Int32 { b.getBaseValue() }

  let d = RefCountedDerived.create()
  expectEqual(takesRefBase(d), 10)

  expectTrue(d.refCount > 0)
  expectTrue(d.refCount < 10)
}

UpcastTestSuite.test("mutation through upcasted reference") {
  let d = Derived.create()
  let b = d as Base
  b.setBaseValue(42)
  defer { b.setBaseValue(1) }

  expectEqual(b.getBaseValue(), 42)
  expectEqual(d.getBaseValue(), 42)
}

UpcastTestSuite.test("Derived? as Base?") {
  let d: Derived? = Derived.create()
  let b: Base? = d
  expectNotNil(b)
  expectEqual(b!.getBaseValue(), 1)
}

UpcastTestSuite.test("Derived?? as Base??") {
  let d: Derived?? = Derived.create()
  let b: Base?? = d
  expectEqual(b!!.getBaseValue(), 1)
}

UpcastTestSuite.test("nil as? Base") {
  let d: Derived? = nil
  let b: Base? = d
  expectNil(b)
}

UpcastTestSuite.test("RefCountedDerived? as RefCountedBase?") {
  let d: RefCountedDerived? = RefCountedDerived.create()
  let b: RefCountedBase? = d
  expectNotNil(b)
  expectEqual(b!.getBaseValue(), 10)

  expectTrue(d!.refCount > 0)
  expectTrue(d!.refCount < 10)
}

UpcastTestSuite.test("RefCountedDerived?? as RefCountedBase??") {
  let d: RefCountedDerived?? = RefCountedDerived.create()
  let b: RefCountedBase?? = d
  expectEqual(b!!.getBaseValue(), 10)

  expectTrue(d!!.refCount > 0)
  expectTrue(d!!.refCount < 10)
}

UpcastTestSuite.test("nil as? RefCountedBase") {
  let d: RefCountedDerived? = nil
  let b: RefCountedBase? = d
  expectNil(b)
}

UpcastTestSuite.test("ternary type unification") {
  let d = Derived.create()
  let b = Base.create()
  d.setBaseValue(123)
  defer { d.setBaseValue(1) }

  let r1 = true ? d : b
  expectEqual(r1.getBaseValue(), 123)

  let r2 = false ? d : b
  expectEqual(r2.getBaseValue(), 1) 
}

UpcastTestSuite.test("ternary refcounted type unification") {
  let d = RefCountedDerived.create()
  let b = RefCountedBase.create()
  let r = true ? d : b
  expectEqual(r.getBaseValue(), 10)

  expectTrue(d.refCount > 0)
  expectTrue(d.refCount < 10)
}

UpcastTestSuite.test("heterogeneous array literal") {
  let d = Derived.create()
  let l = LeafDerived.create()
  d.setBaseValue(50)
  l.setBaseValue(60)
  defer { d.setBaseValue(1) }
  defer { l.setBaseValue(1) }

  let arr: [Base] = [d, l]
  expectEqual(arr.count, 2)
  expectEqual(arr[0].getBaseValue(), 50)
  expectEqual(arr[1].getBaseValue(), 60)
}

UpcastTestSuite.test("heterogeneous array literal refcounted") {
  let d = RefCountedDerived.create()
  let b = RefCountedBase.create()
  let arr: [RefCountedBase] = [d, b]
  expectEqual(arr.count, 2)
  expectEqual(arr[0].getBaseValue(), 10)
  expectEqual(arr[1].getBaseValue(), 10)

  expectTrue(d.refCount > 0)
  expectTrue(d.refCount < 10)
}

protocol Describable {
  func getBaseValue() -> Int32
}

extension Base: Describable {}

UpcastTestSuite.test("protocol conformance inherited") {
  let d = Derived.create()
  let p: Describable = d
  expectEqual(p.getBaseValue(), 1)
}

UpcastTestSuite.test("protocol conformance inherited deeply") {
  let l = LeafDerived.create()
  let p: Describable = l
  expectEqual(p.getBaseValue(), 1)
}

UpcastTestSuite.test("[Protocol]") {
  let b = Base.create()
  let d = Derived.create()
  let l = LeafDerived.create()
  let arr: [Describable] = [b, d, l]
  expectEqual(arr.count, 3)
  for item in arr {
    expectEqual(item.getBaseValue(), 1)
  }
}

extension RefCountedBase: Describable {}

UpcastTestSuite.test("protocol conformance inherited refcounted") {
  let d = RefCountedDerived.create()
  let p: Describable = d
  expectEqual(p.getBaseValue(), 10)

  expectTrue(d.refCount > 0)
  expectTrue(d.refCount < 10)
}

UpcastTestSuite.test("DerivedFromEmptyAndBase as Base") {
  let d = DerivedFromEmptyAndBase.create()
  let b: Base = d as Base
  expectEqual(b.getBaseValue(), 1)
  expectEqual(d.getExtraValue(), 7)
}

UpcastTestSuite.test("DerivedFromEmptyAndBase implicit conversion") {
  func takesBase(_ b: Base) -> Int32 { b.getBaseValue() }
  let d = DerivedFromEmptyAndBase.create()
  expectEqual(takesBase(d), 1)
}

runAllTests()
