// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Atomics

let suite = TestSuite("UnsafeAtomicLazyReference")
defer { runAllTests() }

suite.test("UnsafeAtomicLazyReference<${type}>.create-destroy") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }

  let v = UnsafeAtomicLazyReference<LifetimeTracked>.create()
  defer { v.destroy() }
  expectNil(v.load())
}

suite.test("UnsafeAtomicLazyReference<${type}>.storeIfNil") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }

  do {
    let v = UnsafeAtomicLazyReference<LifetimeTracked>.create()
    expectNil(v.load())

    let ref = LifetimeTracked(42)
    expectTrue(v.initialize(to: ref) === ref)
    expectTrue(v.load() === ref)

    let ref2 = LifetimeTracked(23)
    expectTrue(v.initialize(to: ref2) === ref)
    expectTrue(v.load() === ref)

    v.destroy()
  }
  expectEqual(LifetimeTracked.instances, 0)
}
