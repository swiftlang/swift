// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -o %t/main -g %t/main.swift %S/Inputs/ProbablySafeBufferPointer.swift
// RUN: %target-run %t/main
// REQUIRES: executable_test

// XFAIL: linux

import Darwin
import StdlibUnittest

var suite = TestSuite("ProbablySafeBufferPointer")

suite.test("AssociatedTypes") {
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: ProbablySafeBufferPointer<Float>.self,
    iteratorType: IndexingIterator<ProbablySafeBufferPointer<Float>>.self,
    subSequenceType: Slice<ProbablySafeBufferPointer<Float>>.self,
    indexType: Int.self,
    indicesType: Range<Int>.self)
}

var global: Int = 1
class SomeClass {
  static var staticProp: Int = 1
  final var prop: Int = 1
}

suite.test("global") {
  withUnsafePointer(to: &global) {
    let buffer = ProbablySafeBufferPointer(start: $0, count: 1)
    expectEqual(1, buffer[0])

    _ = ProbablySafeBufferPointer(start: $0, count: 0)
  }
}

suite.test("static") {
  withUnsafePointer(to: &SomeClass.staticProp) {
    let buffer = ProbablySafeBufferPointer(start: $0, count: 1)
    expectEqual(1, buffer[0])

    _ = ProbablySafeBufferPointer(start: $0, count: 0)
  }
}

suite.test("stack") {
  var local = 1
  withUnsafePointer(to: &local) {
    let buffer = ProbablySafeBufferPointer(start: $0, count: 1)
    expectEqual(1, buffer[0])

    _ = ProbablySafeBufferPointer(start: $0, count: 0)
  }
}

suite.test("heap/class") {
  let obj = SomeClass()
  withUnsafePointer(to: &obj.prop) {
    let buffer = ProbablySafeBufferPointer(start: $0, count: 1)
    expectEqual(1, buffer[0])

    _ = ProbablySafeBufferPointer(start: $0, count: 0)
  }
}

suite.test("heap/Array") {
  let numbers = [1, 2, 3]
  numbers.withUnsafeBufferPointer {
    let buffer = ProbablySafeBufferPointer($0)
    expectEqual(1, buffer[0])
    expectEqual(2, buffer[1])
    expectEqual(3, buffer[2])
    expectEqual([1, 2, 3], Array(buffer))

    buffer.withUnsafeBufferPointer {
      expectEqual([1, 2, 3], Array($0))
    }

    let manualSlice = ProbablySafeBufferPointer(start: $0.baseAddress! + 1,
                                                count: 2)
    expectEqual(2, manualSlice[0])
    expectEqual(3, manualSlice[1])
    expectEqual([2, 3], Array(manualSlice))

    manualSlice.withUnsafeBufferPointer {
      expectEqual([2, 3], Array($0))
    }

    _ = ProbablySafeBufferPointer(start: $0.baseAddress, count: 0)
  }
}

suite.test("heap/manual") {
  let numbers = UnsafeMutableBufferPointer<Int>.allocate(capacity: 3)
  defer { numbers.deallocate() }
  numbers[0] = 1
  numbers[1] = 2
  numbers[2] = 3

  let buffer = ProbablySafeBufferPointer(UnsafeBufferPointer(numbers))
  expectEqual(1, buffer[0])
  expectEqual(2, buffer[1])
  expectEqual(3, buffer[2])
  expectEqual([1, 2, 3], Array(buffer))

  let manualSlice = ProbablySafeBufferPointer(start: numbers.baseAddress! + 1,
                                              count: 2)
  expectEqual(2, manualSlice[0])
  expectEqual(3, manualSlice[1])
  expectEqual([2, 3], Array(manualSlice))

  _ = ProbablySafeBufferPointer(start: numbers.baseAddress, count: 0)
}

func onAnotherThread(_ operation: () -> Void) {
  withoutActuallyEscaping(operation) { operation in
    // To get a stable address.
    var operationVar = operation
    withUnsafeMutablePointer(to: &operationVar) { operationPtr in
      var threadID: pthread_t? = nil
      let status = pthread_create(&threadID, nil, { rawOperationPtr in
        // All this effort just to invoke the original closure.
        rawOperationPtr.assumingMemoryBound(to: (() -> Void).self).pointee()
        return nil
      }, operationPtr)
      precondition(status == 0, "failed to create thread")

      let joinStatus = pthread_join(threadID!, nil)
      precondition(joinStatus == 0, "failed to join thread")
    }
  }
}

suite.test("thread/init") {
  var local = 1
  withUnsafePointer(to: &local) { addr in
    onAnotherThread {
      _ = ProbablySafeBufferPointer(start: addr, count: 0)
      expectCrashLater()
      _ = ProbablySafeBufferPointer(start: addr, count: 1)
    }
  }
}

suite.test("thread/access") {
  var local = 1
  withUnsafePointer(to: &local) { addr in
    let empty = ProbablySafeBufferPointer(start: addr, count: 0)
    let buffer = ProbablySafeBufferPointer(start: addr, count: 1)
    onAnotherThread {
      empty.withUnsafeBufferPointer {
        expectEqual(0, $0.count)
      }
      expectCrashLater()
      buffer.withUnsafeBufferPointer { _ in
        expectUnreachable()
      }
    }
  }
}

suite.test("out-of-bounds") {
  let numbers = UnsafeMutableBufferPointer<Int>.allocate(capacity: 3)
  defer { numbers.deallocate() }

  expectCrashLater()
  _ = ProbablySafeBufferPointer(start: numbers.baseAddress, count: 50)
}

suite.test("across-allocations") {
  let numbers = UnsafeMutableBufferPointer<Int>.allocate(capacity: 3)

  expectCrashLater()
  _ = ProbablySafeBufferPointer(start: numbers.baseAddress! - 1, count: 2)
}

runAllTests()
