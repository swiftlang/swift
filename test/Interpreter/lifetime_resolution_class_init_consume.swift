// RUN: %target-run-simple-swift(-Xfrontend -enable-lifetime-resolution)

// REQUIRES: executable_test

import StdlibUnittest

var ConsumeInInitTestSuite = TestSuite("ConsumeInInit")

struct E: Error {}

var deinitCalled = 0
var consumedValue = 0

class ConsumeThenThrow {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init(fail: Bool) throws {
    x = LifetimeTracked(1)
    y = LifetimeTracked(2)
    let old = consume x
    consumedValue = old.value
    if fail {
      throw E()
    }
    x = LifetimeTracked(old.value + 1)
  }

  deinit {
    deinitCalled += 1
  }
}

ConsumeInInitTestSuite.test("ConsumeThenThrow") {
  deinitCalled = 0
  consumedValue = 0

  expectNil(try? ConsumeThenThrow(fail: true))
  expectEqual(1, consumedValue)
  expectEqual(0, deinitCalled)
  expectEqual(0, LifetimeTracked.instances)

  do {
    let c = try ConsumeThenThrow(fail: false)
    expectEqual(2, c.x.value)
    expectEqual(2, c.y.value)
    expectEqual(2, LifetimeTracked.instances)
  } catch {
    expectUnreachable()
  }
  expectEqual(1, deinitCalled)
  expectEqual(0, LifetimeTracked.instances)
}

class ConsumeThenFail {
  var x: LifetimeTracked
  var y: LifetimeTracked

  init?(fail: Bool) {
    x = LifetimeTracked(1)
    y = LifetimeTracked(2)
    _ = consume y
    if fail {
      return nil
    }
    y = LifetimeTracked(3)
  }

  deinit {
    deinitCalled += 1
  }
}

ConsumeInInitTestSuite.test("ConsumeThenFail") {
  deinitCalled = 0

  expectNil(ConsumeThenFail(fail: true))
  expectEqual(0, deinitCalled)
  expectEqual(0, LifetimeTracked.instances)

  do {
    let c = ConsumeThenFail(fail: false)
    expectEqual(3, c?.y.value)
  }
  expectEqual(1, deinitCalled)
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()
