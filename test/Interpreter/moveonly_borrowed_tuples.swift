// RUN: %target-run-simple-swift(-enable-experimental-feature MoveOnlyTuples -Xfrontend -sil-verify-all)
// RUN: %target-run-simple-swift(-O -enable-experimental-feature MoveOnlyTuples -Xfrontend -sil-verify-all)

// REQUIRES: executable_test
// REQUIRES: swift_feature_MoveOnlyTuples

import StdlibUnittest

var tests = TestSuite("BorrowedNoncopyableTuples")

final class Tracker {
  var destroyed: [Int] = []
}

struct Value: ~Copyable {
  let number: Int
  let tracker: Tracker

  deinit { tracker.destroyed.append(number) }
}

@inline(never)
func read(_ value: borrowing Value) -> Int { value.number }

@inline(never)
func readTuple(_ value: borrowing (Value, Int)) -> Int {
  read(value.0) + read(value.0) + value.1
}

@inline(never)
func readNested(_ value: borrowing (Value, (Value, Int))) -> Int {
  read(value.0) + read(value.1.0) + value.1.1
}

protocol Callback<Arguments>: ~Copyable {
  associatedtype Arguments: ~Copyable
  func call(_ arguments: borrowing Arguments) -> Int
}

struct TupleCallback: Callback, ~Copyable {
  func call(_ arguments: borrowing (Value, Int)) -> Int {
    readTuple(arguments)
  }
}

@inline(never)
func call<C: Callback & ~Copyable>(
  _ callback: borrowing C, _ arguments: borrowing C.Arguments
) -> Int where C.Arguments: ~Copyable {
  callback.call(arguments)
}

@inline(never)
func take(_ value: consuming Value) {}

@inline(never)
func partialConsume(_ value: consuming (Value, Value)) {
  take(value.0)
  expectEqual(2, read(value.1))
}

@inline(never)
func makeTuple(_ tracker: Tracker) -> (Value, Int) {
  (Value(number: 1, tracker: tracker), 2)
}

enum Failure: Error {
  case expected
}

@inline(never)
func borrowAndThrow(_ value: borrowing (Value, Int)) throws {
  expectEqual(4, readTuple(value))
  throw Failure.expected
}

tests.test("borrow preserves the owner") {
  let tracker = Tracker()
  do {
    let value = (Value(number: 3, tracker: tracker), 4)
    expectEqual(10, readTuple(value))
    expectEqual(10, readTuple(value))
    expectEqual([], tracker.destroyed)
    expectEqual(3, read(value.0))
  }
  expectEqual([3], tracker.destroyed)
}

tests.test("nested tuples") {
  let tracker = Tracker()
  do {
    let value = (Value(number: 1, tracker: tracker),
                 (Value(number: 2, tracker: tracker), 3))
    expectEqual(6, readNested(value))
    expectEqual(6, readNested(value))
    expectEqual([], tracker.destroyed)
    expectEqual(6, readNested(value))
  }
  expectEqual([1, 2], tracker.destroyed.sorted())
}

tests.test("generic callback witness preserves the owner") {
  let tracker = Tracker()
  let callback = TupleCallback()
  do {
    let value = (Value(number: 1, tracker: tracker), 2)
    expectEqual(4, call(callback, value))
    expectEqual(4, call(callback, value))
    expectEqual([], tracker.destroyed)
    expectEqual(4, readTuple(value))
  }
  expectEqual([1], tracker.destroyed)
}

tests.test("partial consumption destroys each element once") {
  let tracker = Tracker()
  partialConsume((Value(number: 1, tracker: tracker),
                  Value(number: 2, tracker: tracker)))
  expectEqual([1, 2], tracker.destroyed.sorted())
}

tests.test("borrowed tuple literals") {
  let tracker = Tracker()
  do {
    let value = Value(number: 1, tracker: tracker)
    expectEqual(4, readTuple((value, 2)))
    expectEqual(5, readTuple((value, 3)))
    expectEqual([], tracker.destroyed)
    expectEqual(1, read(value))
  }
  expectEqual([1], tracker.destroyed)
}

tests.test("temporary tuple projections") {
  let tracker = Tracker()
  expectEqual(1, read(makeTuple(tracker).0))
  expectEqual([1], tracker.destroyed)
  take(makeTuple(tracker).0)
  expectEqual([1, 1], tracker.destroyed)
  expectEqual(2, makeTuple(tracker).1)
  expectEqual([1, 1, 1], tracker.destroyed)
}

tests.test("throwing borrow preserves the owner") {
  let tracker = Tracker()
  do {
    let value = (Value(number: 1, tracker: tracker), 2)
    do {
      try borrowAndThrow(value)
      expectUnreachable()
    } catch Failure.expected {
    } catch {
      expectUnreachable()
    }
    expectEqual([], tracker.destroyed)
    expectEqual(4, readTuple(value))
  }
  expectEqual([1], tracker.destroyed)
}

runAllTests()
