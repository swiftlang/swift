// RUN: %target-run-simple-swift
//
// Also compile at -O with the SIL verifier on, so that the open Mem2Reg bug
// at swiftlang/swift#89533 is visible on every CI run until it's fixed.
// Today: this RUN line crashes the compiler on `take()` (any payload type)
// and the test fails.  Once the upstream fix lands, both RUN lines succeed
// and this becomes a permanent regression guard.
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %s -o /dev/null

// REQUIRES: executable_test
// REQUIRES: synchronization

import Synchronization
import StdlibUnittest

let suite = TestSuite("Disconnected")

final class Box {
  var value: Int
  init(_ value: Int) { self.value = value }
}

struct Counter: ~Copyable {
  let id: Int
  static var liveCount = 0

  init(id: Int) {
    self.id = id
    Counter.liveCount += 1
  }

  deinit {
    Counter.liveCount -= 1
  }
}

if #available(SwiftStdlib 6.5, *) {
  suite.test("take returns wrapped value") {
    let wrapper = Disconnected(Box(42))
    let value = wrapper.take()
    expectEqual(42, value.value)
  }

  suite.test("swap returns previous value and stores new one") {
    var wrapper = Disconnected(Box(1))
    let previous = wrapper.swap(newValue: Box(2))
    expectEqual(1, previous.value)
    expectEqual(2, wrapper.take().value)
  }

  suite.test("swap is discardable") {
    var wrapper = Disconnected(Box(1))
    wrapper.swap(newValue: Box(2))
    expectEqual(2, wrapper.take().value)
  }

  suite.test("withValue can mutate in place") {
    var wrapper = Disconnected(Box(10))
    let returned = wrapper.withValue { value in
      value.value += 5
      return value.value
    }
    expectEqual(15, returned)
    expectEqual(15, wrapper.take().value)
  }

  suite.test("withValue propagates typed throws and restores wrapper") {
    enum E: Error { case boom }
    var wrapper = Disconnected(Box(7))

    do {
      try wrapper.withValue { value throws(E) -> Void in
        value.value = 99
        throw E.boom
      }
      expectUnreachable("withValue should have thrown")
    } catch E.boom {
      // Expected.
    } catch {
      expectUnreachable("unexpected error: \(error)")
    }

    // The wrapper must still hold the (mutated) value after the throw.
    expectEqual(99, wrapper.take().value)
  }

  suite.test("works with ~Copyable Value: take consumes exactly once") {
    expectEqual(0, Counter.liveCount)
    do {
      let wrapper = Disconnected(Counter(id: 1))
      expectEqual(1, Counter.liveCount)
      let value = wrapper.take()
      expectEqual(1, Counter.liveCount)
      _ = consume value
    }
    expectEqual(0, Counter.liveCount)
  }

  suite.test("works with ~Copyable Value: swap drops the old value") {
    expectEqual(0, Counter.liveCount)
    do {
      var wrapper = Disconnected(Counter(id: 1))
      expectEqual(1, Counter.liveCount)
      let old = wrapper.swap(newValue: Counter(id: 2))
      expectEqual(2, Counter.liveCount)
      _ = consume old
      expectEqual(1, Counter.liveCount)
      _ = consume wrapper
    }
    expectEqual(0, Counter.liveCount)
  }

  suite.test("works with ~Copyable Value: withValue restores on throw") {
    enum E: Error { case boom }
    expectEqual(0, Counter.liveCount)
    do {
      var wrapper = Disconnected(Counter(id: 1))
      do {
        try wrapper.withValue { value throws(E) -> Void in
          throw E.boom
        }
        expectUnreachable("withValue should have thrown")
      } catch {
        // Expected.
      }
      expectEqual(1, Counter.liveCount)
      _ = consume wrapper
    }
    expectEqual(0, Counter.liveCount)
  }

  // Regression guard: primitive `Value` types exercise the trivially-sized /
  // zero-sized paths through the wrapper's `consume`-based SIL. If the SIL
  // verifier rejects the lowering of `consume _value` on a trivial field, or
  // IRGen mishandles a zero-size payload, these tests will fail to compile or
  // produce wrong values.
  suite.test("primitive Value: Int round-trips through take") {
    let wrapper = Disconnected(42)
    expectEqual(42, wrapper.take())
  }

  suite.test("primitive Value: Int swap and withValue") {
    var wrapper = Disconnected(1)
    expectEqual(1, wrapper.swap(newValue: 2))
    let result = wrapper.withValue { value -> Int in
      value &+= 10
      return value
    }
    expectEqual(12, result)
    expectEqual(12, wrapper.take())
  }

  suite.test("primitive Value: Void round-trips") {
    let wrapper = Disconnected(())
    wrapper.take()
  }

  suite.test("primitive Value: Void swap and withValue") {
    var wrapper = Disconnected(())
    wrapper.swap(newValue: ())
    wrapper.withValue { _ in }
    wrapper.take()
  }

  suite.test("primitive Value: Optional<Int>") {
    var wrapper = Disconnected(Int?.some(7))
    expectEqual(.some(7), wrapper.swap(newValue: nil))
    expectNil(wrapper.take())
  }

  suite.test("primitive Value: tuple of trivials") {
    let wrapper = Disconnected((11, 22))
    let (a, b) = wrapper.take()
    expectEqual(11, a)
    expectEqual(22, b)
  }
}

runAllTests()
