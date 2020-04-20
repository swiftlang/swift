// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Atomics

let suite = TestSuite("AtomicRawRepresentable")
defer { runAllTests() }

enum State: Int, AtomicValue {
  case starting
  case running
  case stopped
}


suite.test("load") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let v = UnsafeAtomic<State>.create(initialValue: .starting)
  defer { v.destroy() }
  expectEqual(v.load(ordering: .relaxed), State.starting)
}

suite.test("store") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let v = UnsafeAtomic<State>.create(initialValue: .starting)
  defer { v.destroy() }
  expectEqual(State.starting, v.load(ordering: .relaxed))
  v.store(.running, ordering: .relaxed)
  expectEqual(State.running, v.load(ordering: .relaxed))
}

suite.test("exchange") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let v = UnsafeAtomic<State>.create(initialValue: .starting)
  defer { v.destroy() }
  expectEqual(State.starting, v.load(ordering: .relaxed))
  expectEqual(State.starting, v.exchange(.running, ordering: .relaxed))
  expectEqual(State.running, v.load(ordering: .relaxed))
  expectEqual(State.running, v.exchange(.stopped, ordering: .relaxed))
  expectEqual(State.stopped, v.load(ordering: .relaxed))
}

suite.test("compareExchange") {
  guard #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) else {
    return
  }
  let v = UnsafeAtomic<State>.create(initialValue: .starting)
  defer { v.destroy() }
  expectEqual(State.starting, v.load(ordering: .relaxed))

  var (success, old) = v.compareExchange(
    expected: .starting,
    desired: .running,
    ordering: .relaxed)
  expectTrue(success)
  expectEqual(State.starting, old)
  expectEqual(State.running, v.load(ordering: .relaxed))

  (success, old) = v.compareExchange(
    expected: .starting,
    desired: .stopped,
    ordering: .relaxed)
  expectFalse(success)
  expectEqual(.running, old)
  expectEqual(State.running, v.load(ordering: .relaxed))

  (success, old) = v.compareExchange(
    expected: .running,
    desired: .stopped,
    ordering: .relaxed)
  expectTrue(success)
  expectEqual(State.running, old)
  expectEqual(State.stopped, v.load(ordering: .relaxed))
}
