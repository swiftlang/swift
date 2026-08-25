// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test
// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import SuperVirtualDispatch
import StdlibUnittest

var SuperVirtualDispatchTests = TestSuite("super.virtualMethod() dispatch for FRTs")

extension Derived {
  public func callSuperVirtual() -> Int32 {
    return super.virtualMethod()
  }
}

extension LeafDerived {
  public func callSuperVirtualFromLeaf() -> Int32 {
    return super.virtualMethod()
  }
}

extension LeafOverNoOverride {
  public func callSuperVirtualFromLeafOverNoOverride() -> Int32 {
    return super.virtualMethod()
  }
}

extension DerivedWithUnrelatedBase {
  public func callSuperVirtualFromMultiBase() -> Int32 {
    return super.virtualMethod()
  }
}

extension DerivedWithConflictingBase {
  public func callSuperVirtualFromConflictingBase() -> Int32 {
    return super.virtualMethod()
  }
}

extension Derived {
  public func callSuperVirtualViaValue(_ b: Bool) -> Int32 {
    let toCall = b ? self.virtualMethod : super.virtualMethod
    return toCall()
  }
}

SuperVirtualDispatchTests.test("super.virtualMethod() dispatches statically to the base implementation") {
  let derived = Derived.create()
  expectEqual(200, derived.virtualMethod())
  expectEqual(100, derived.callSuperVirtual())
}

SuperVirtualDispatchTests.test("super.virtualMethod() resolves to the immediate base across multiple inheritance levels") {
  let leaf = LeafDerived.create()
  expectEqual(300, leaf.virtualMethod())
  expectEqual(200, leaf.callSuperVirtualFromLeaf())
}

SuperVirtualDispatchTests.test("super.virtualMethod() resolves to a method the immediate base inherits from its own base") {
  let leaf = LeafOverNoOverride.create()
  expectEqual(400, leaf.virtualMethod())
  expectEqual(100, leaf.callSuperVirtualFromLeafOverNoOverride())
}

SuperVirtualDispatchTests.test("super.virtualMethod() dispatches to the primary FRT base when there is also an unrelated base") {
  let derived = DerivedWithUnrelatedBase.create()
  expectEqual(500, derived.virtualMethod())
  expectEqual(100, derived.callSuperVirtualFromMultiBase())
}

SuperVirtualDispatchTests.test("super.virtualMethod() dispatches to the primary FRT base when a secondary base declares the same method") {
  let derived = DerivedWithConflictingBase.create()
  expectEqual(600, derived.virtualMethod())
  expectEqual(100, derived.callSuperVirtualFromConflictingBase())
}

SuperVirtualDispatchTests.test("an unapplied super.virtualMethod reference statically dispatches to the base") {
  let derived = Derived.create()
  expectEqual(200, derived.callSuperVirtualViaValue(true))
  expectEqual(100, derived.callSuperVirtualViaValue(false))
}

runAllTests()
