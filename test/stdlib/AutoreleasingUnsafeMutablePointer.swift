// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import ObjectiveC // for autoreleasepool

var suite = TestSuite("AutoreleasingUnsafeMutablePointer")

/// Call `body` passing it an AutoreleasingUnsafeMutablePointer whose pointee
/// has the specified value, allocated in a temporary buffer.
func withAUMP<Pointee: AnyObject>(
  as type: Pointee.Type = Pointee.self,
  initialValue: Optional<Unmanaged<Pointee>> = nil,
  _ body: (AutoreleasingUnsafeMutablePointer<Optional<Pointee>>) -> Void
) {
  let p =
    UnsafeMutablePointer<Optional<Unmanaged<Pointee>>>.allocate(capacity: 1)
  p.initialize(to: initialValue)
  body(AutoreleasingUnsafeMutablePointer(p))
  p.deallocate()
}

/// Call `body` passing it an AutoreleasingUnsafeMutablePointer whose pointee
/// has the specified value, allocated in a temporary buffer.
func withAUMP<Pointee: AnyObject>(
  initialValues: [Unmanaged<Pointee>?],
  _ body: (AutoreleasingUnsafeMutablePointer<Optional<Pointee>>) -> Void
) {
  let p = UnsafeMutablePointer<Optional<Unmanaged<Pointee>>>.allocate(
    capacity: initialValues.count
  )
  for i in 0 ..< initialValues.count {
    (p + i).initialize(to: initialValues[i])
  }

  let aump = AutoreleasingUnsafeMutablePointer<Optional<Pointee>>(p)
  body(aump)
  p.deallocate()
}

suite.test("helper calls its closure exactly once") {
  // `withAUMP` is expected to call its closure exactly once, with an argument
  // pointing to a nil value.
  var runCount = 0
  withAUMP(as: LifetimeTracked.self) { p in
    runCount += 1
    expectNil(p.pointee)
  }
  expectEqual(runCount, 1)
}

suite.test("init doesn't autorelease") {
  let originalInstances = LifetimeTracked.instances
  let unmanaged = Unmanaged.passRetained(LifetimeTracked(42))
  expectEqual(LifetimeTracked.instances, originalInstances + 1)

  withAUMP(initialValue: unmanaged) { p in noop(p) }

  // Releasing the original reference should immediately deallocate the
  // object.
  expectEqual(LifetimeTracked.instances, originalInstances + 1)
  unmanaged.release()
  expectEqual(LifetimeTracked.instances, originalInstances)
}

suite.test("getter initially returns the initial value") {
  withAUMP(as: LifetimeTracked.self) { p in
    expectNil(p.pointee)
  }

  let unmanaged = Unmanaged.passRetained(LifetimeTracked(42))
  withAUMP(initialValue: unmanaged) { p in
    expectTrue(p.pointee === unmanaged.takeUnretainedValue())
  }
  unmanaged.release()
}

suite.test("pointee getter returns the last value set") {
  autoreleasepool {
    withAUMP(as: LifetimeTracked.self) { p in
      expectNil(p.pointee)
      let object = LifetimeTracked(23)
      p.pointee = object
      expectTrue(p.pointee === object)
      let other = LifetimeTracked(42)
      p.pointee = other
      expectTrue(p.pointee === other)
      p.pointee = nil
      expectNil(p.pointee)
    }
  }
}

suite.test("pointee getter doesn't autorelease") {
  let originalInstances = LifetimeTracked.instances
  autoreleasepool {
    let unmanaged = Unmanaged.passRetained(LifetimeTracked(42))
    expectEqual(LifetimeTracked.instances, originalInstances + 1)

    withAUMP(initialValue: unmanaged) { p in
      expectTrue(p.pointee === unmanaged.takeUnretainedValue())
    }
    // Releasing the original reference should immediately deallocate the
    // object.
    expectEqual(LifetimeTracked.instances, originalInstances + 1)
    unmanaged.release()
    expectEqual(LifetimeTracked.instances, originalInstances)
  }
}

suite.test("pointee setter autoreleases the new value") {
  let originalInstances = LifetimeTracked.instances
  autoreleasepool {
    withAUMP(as: LifetimeTracked.self) { p in
      let object = LifetimeTracked(42)
      // There is one more instance now.
      expectEqual(LifetimeTracked.instances, originalInstances + 1)
      p.pointee = object
    }
    // The strong reference to `object` is gone, but the autoreleasepool
    // is still holding onto it.
    expectEqual(LifetimeTracked.instances, originalInstances + 1)
  }
  // Draining the pool deallocates the instance.
  expectEqual(LifetimeTracked.instances, originalInstances)
}

suite.test("AutoreleasingUnsafeMutablePointer doesn't retain its value") {
  let originalInstances = LifetimeTracked.instances
  withAUMP(as: LifetimeTracked.self) { p in
    autoreleasepool {
      let object = LifetimeTracked(42)
      p.pointee = object
      expectEqual(LifetimeTracked.instances, originalInstances + 1)
    }
    // Draining the pool deallocates the instance, even though
    // the autoreleasing pointer still points to it.
    // This is okay as long as the pointer isn't dereferenced.
    expectEqual(LifetimeTracked.instances, originalInstances)
  }
  expectEqual(LifetimeTracked.instances, originalInstances)
}

suite.test("subscript[0] is the same as pointee.getter") {
  withAUMP(as: LifetimeTracked.self) { p in
    expectNil(p[0])
  }

  let unmanaged = Unmanaged.passRetained(LifetimeTracked(42))
  withAUMP(initialValue: unmanaged) { p in
    expectTrue(p[0] === unmanaged.takeUnretainedValue())
  }
  unmanaged.release()
}


suite.test("subscript and advanced(by:) works") {
  let originalInstances = LifetimeTracked.instances
  let count = 100

  var refs = 0
  let values: [Unmanaged<LifetimeTracked>?] = (0 ..< count).map { i in
    if i % 10 == 0 {
      refs += 1
      return Unmanaged.passRetained(LifetimeTracked(i))
    } else {
      return nil
    }
  }

  withAUMP(initialValues: values) { p in
    for i in 0 ..< count {
      expectTrue(p[i] === values[i]?.takeUnretainedValue())
      expectTrue(p.advanced(by: i).pointee === values[i]?.takeUnretainedValue())
    }
  }

  expectEqual(LifetimeTracked.instances, originalInstances + refs)
  for unmanaged in values {
    unmanaged?.release()
  }
  expectEqual(LifetimeTracked.instances, originalInstances)
}

runAllTests()
