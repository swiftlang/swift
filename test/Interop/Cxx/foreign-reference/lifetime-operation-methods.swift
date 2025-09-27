// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -I %swift_src_root/lib/ClangImporter/SwiftBridging -Xfrontend -disable-availability-checking)

// REQUIRES: executable_test

// Temporarily disable when running with an older runtime (rdar://128681137)
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import LifetimeOperationMethods

var LifetimeMethodsTestSuite = TestSuite("Lifetime operations that are instance methods")

LifetimeMethodsTestSuite.test("retain/release methods") {
  let a = RefCountedBox(123)
  expectEqual(a.value, 123)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

LifetimeMethodsTestSuite.test("retain/release methods from base type") {
  let a = DerivedRefCountedBox(321, 456)
  expectEqual(a.value, 321)
  expectEqual(a.secondValue, 456)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number

  a.secondValue = 789
  expectEqual(a.secondValue, 789)
}

LifetimeMethodsTestSuite.test("retain in base type, release in derived type") {
  let a = DerivedHasRelease(321)
  expectEqual(a.value, 321)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

LifetimeMethodsTestSuite.test("retain in base type, release in derived templated type") {
  let a = TemplatedDerivedHasReleaseInt(456)
  expectEqual(a.value, 456)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number

  let b = TemplatedDerivedHasReleaseFloat(5.66)
  expectEqual(b.value, 5.66)
}

LifetimeMethodsTestSuite.test("CRTP") {
  let a = CRTPDerived(789)
  expectEqual(a.value, 789)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

LifetimeMethodsTestSuite.test("virtual retain/release") {
  let a = VirtualRetainRelease(456)
  expectEqual(a.value, 456)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

LifetimeMethodsTestSuite.test("overridden virtual retain/release") {
  let a = DerivedVirtualRetainRelease(456)
  expectEqual(a.value, 456)
  expectFalse(a.calledBase) // in optimized builds, we might not call retain/release at all
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

LifetimeMethodsTestSuite.test("overridden pure virtual retain/release") {
  let a = DerivedPureVirtualRetainRelease(789)
  expectEqual(a.value, 789)
  expectTrue(a.refCount > 0)
  expectTrue(a.refCount < 10) // optimizations would affect the exact number
}

runAllTests()
