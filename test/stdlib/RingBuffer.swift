// RUN: %target-run-simple-swift
// REQUIRES: executable_test
import StdlibUnittest

let RingBufferTests = TestSuite("RingBuffer")

RingBufferTests.test("CapacityInit") {
  let ringBuffer = RingBuffer<Int>(capacity: 3)
  expectEqual(ringBuffer.capacity, 3)
}

// RingBuffer common method tests

RingBufferTests.test("PushBack") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushBack(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.pushBack(4)
  expectEqual(ringBuffer.map { $0 }, [2, 3, 4])
  ringBuffer.pushBack(5)
  expectEqual(ringBuffer.map { $0 }, [3, 4, 5])
  ringBuffer.pushBack(6)
  expectEqual(ringBuffer.map { $0 }, [4, 5, 6])
}


RingBufferTests.test("TestPopBack") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushBack(1)
  expectEqual(ringBuffer.popBack(), 1)
  expectEqual(ringBuffer.map { $0 }, [])
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.pushBack(4)
  ringBuffer.pushBack(5)
  expectEqual(ringBuffer.map { $0 }, [3, 4, 5])
  expectEqual(ringBuffer.popBack(), 5)
  expectEqual(ringBuffer.map { $0 }, [3, 4])
  expectEqual(ringBuffer.popBack(), 4)
  expectEqual(ringBuffer.popBack(), 3)
  expectEqual(ringBuffer.map { $0 }, [])
}

RingBufferTests.test("TestPushFront") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushFront(2)
  ringBuffer.pushFront(3)
  expectEqual(ringBuffer.map { $0 }, [3, 2, 1])
  ringBuffer.pushFront(4)
  expectEqual(ringBuffer.map { $0 }, [4, 3, 2])
  ringBuffer.pushFront(5)
  expectEqual(ringBuffer.map { $0 }, [5, 4, 3])
  ringBuffer.pushFront(6)
  expectEqual(ringBuffer.map { $0 }, [6, 5, 4])
}

RingBufferTests.test("TestPopFront") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushFront(2)
  ringBuffer.pushFront(3)
  expectEqual(ringBuffer.map { $0 }, [3, 2, 1])
  expectEqual(ringBuffer.popFront(), 3)
  expectEqual(ringBuffer.map { $0 }, [2, 1])
  ringBuffer.pushFront(5)
  ringBuffer.pushFront(6)
  expectEqual(ringBuffer.map { $0 }, [6, 5, 2])
  expectEqual(ringBuffer.popFront(), 6)
  expectEqual(ringBuffer.popFront(), 5)
  expectEqual(ringBuffer.map { $0 }, [2])
  expectEqual(ringBuffer.popFront(), 2)
  expectEqual(ringBuffer.map { $0 }, [])
}

RingBufferTests.test("TestFrontBackPushPop") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.pushFront(4)
  expectEqual(ringBuffer.map { $0 }, [4, 1, 2])
  ringBuffer.pushBack(5)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 5])
  expectEqual(ringBuffer.popBack(), 5)
  expectEqual(ringBuffer.map { $0 }, [1, 2])
  expectEqual(ringBuffer.popFront(), 1)
  expectEqual(ringBuffer.map { $0 }, [2])
  expectEqual(ringBuffer.popBack(), 2)
  expectEqual(ringBuffer.map { $0 }, [])
}

// RingBuffer capacity tests

RingBufferTests.test("TestResizeCapacityToZero") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.isFull, true)
  ringBuffer.resize(newCapacity: 0)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.isEmpty, true)
  expectEqual(ringBuffer.map { $0 }, [])
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.isEmpty, true)
  expectEqual(ringBuffer.map { $0 }, [])
}

RingBufferTests.test("TestResizeCapacityEqual") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.resize(newCapacity: 3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  expectEqual(ringBuffer.isFull, true)
  ringBuffer.pushBack(4)
  expectEqual(ringBuffer.map { $0 }, [2, 3, 4])
}

RingBufferTests.test("TestResizeCapacityHigher") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  expectEqual(ringBuffer.isFull, true)
  ringBuffer.pushBack(4)
  expectEqual(ringBuffer.map { $0 }, [2, 3, 4])
  ringBuffer.resize(newCapacity: 5)
  expectEqual(ringBuffer.capacity, 5)
  ringBuffer.pushFront(5)
  ringBuffer.pushBack(6)
  expectEqual(ringBuffer.map { $0 }, [5, 2, 3, 4, 6])
  expectEqual(ringBuffer.isFull, true)
  ringBuffer.pushBack(7)
  ringBuffer.pushBack(8)
  expectEqual(ringBuffer.map { $0 }, [3, 4, 6, 7, 8])
}

RingBufferTests.test("TestResizeCapacityLower") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  expectEqual(ringBuffer.isFull, true)
  ringBuffer.resize(newCapacity: 1)
  expectEqual(ringBuffer.capacity, 1)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.map { $0 }, [1])
}

// RingBuffer RandomAccessCollection method tests

RingBufferTests.test("TestRandomAccessCollectionMethods") {
  var ringBuffer = RingBuffer<Int>(capacity: 10)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.startIndex, 0)
  expectEqual(ringBuffer.endIndex, 3)
  expectEqual(ringBuffer[0], 1)
  expectEqual(ringBuffer[1], 2)
  expectEqual(ringBuffer[0...2], [1, 2, 3])
  ringBuffer.pushFront(4)
  expectEqual(ringBuffer[0...2], [4, 1, 2])
  ringBuffer.pushFront(5)
  expectEqual(ringBuffer[0...2], [5, 4, 1])
  expectEqual(ringBuffer.map { $0 }, [5, 4, 1, 2, 3])
}

// RingBuffer RangeReplaceableCollection method tests

RingBufferTests.test("TestEmptyInit") {
  let ringBuffer = RingBuffer<Int>()
  expectEqual(ringBuffer.isEmpty, true)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.capacity, 0)
}

RingBufferTests.test("TestSequenceInit") {
  let testInput = [1, 2, 3]
  var ringBuffer = RingBuffer<Int>(testInput)
  expectEqual(ringBuffer.isEmpty, false)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.capacity, 3)
  expectEqual(ringBuffer.count, 3)
  ringBuffer.pushBack(4)
  expectEqual(ringBuffer.map { $0 }, [2, 3, 4])
  ringBuffer.pushFront(5)
  expectEqual(ringBuffer.map { $0 }, [5, 2, 3])
}

RingBufferTests.test("TestReplaceSubrangeNewElementsEmpty") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.replaceSubrange(0..<1, with: [])
  expectEqual(ringBuffer.map { $0 }, [2, 3])
  ringBuffer.replaceSubrange(0..<1, with: [])
  expectEqual(ringBuffer.map { $0 }, [3])
  ringBuffer.replaceSubrange(0..<1, with: [])
  expectEqual(ringBuffer.map { $0 }, [])
}

RingBufferTests.test("TestReplaceSubrangeRangeSizeEqualCollectionSize") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.replaceSubrange(0..<1, with: [4])
  expectEqual(ringBuffer.map { $0 }, [4, 2, 3])
  ringBuffer.replaceSubrange(1..<2, with: [5])
  expectEqual(ringBuffer.map { $0 }, [4, 5, 3])
  ringBuffer.replaceSubrange(2..<3, with: [6])
  expectEqual(ringBuffer.map { $0 }, [4, 5, 6])
  ringBuffer.pushBack(7)
  expectEqual(ringBuffer.map { $0 }, [5, 6, 7])
  ringBuffer.pushFront(8)
  expectEqual(ringBuffer.map { $0 }, [8, 5, 6])
}

/// FIXME: UPDATE REPLACE SUBRANGE TESTS WITH MORE EDGE CASES

RingBufferTests.test("TestReplaceSubrageRangeSizeLowerCollectionSize") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.replaceSubrange(0..<1, with: [1, 2, 3])
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3, 2, 3])
  expectEqual(ringBuffer.capacity, 5)
  expectEqual(ringBuffer.isFull, true)
}

RingBufferTests.test("TestReplaceSubrageRangeSizeHigherCollectionSize") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.replaceSubrange(0..<2, with: [4])
  expectEqual(ringBuffer.map { $0 }, [4, 3])
  expectEqual(ringBuffer.capacity, 3)
}

RingBufferTests.test("TestRemoveSubrange") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  ringBuffer.removeSubrange(0..<1)
  expectEqual(ringBuffer.map { $0 }, [2, 3])
  ringBuffer.removeSubrange(0..<1)
  expectEqual(ringBuffer.map { $0 }, [3])
  ringBuffer.removeSubrange(0..<1)
  expectEqual(ringBuffer.map { $0 }, [])
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
}

RingBufferTests.test("TestRemoveSubrangeSecond") {
  var firstTest = RingBuffer<Int>(arrayLiteral: 1, 2, 3, 4, 5)
  firstTest.removeSubrange(0..<2)
  expectEqual(firstTest.map { $0 }, [3, 4, 5])
  var secondTest = RingBuffer<Int>(arrayLiteral: 1, 2, 3, 4, 5)
  secondTest.removeSubrange(1..<3)
  expectEqual(secondTest.map { $0 }, [1, 4, 5])
  var thirdTest = RingBuffer<Int>(arrayLiteral: 1, 2, 3, 4, 5)
  thirdTest.removeSubrange(3..<5)
  expectEqual(thirdTest.map { $0 }, [1, 2, 3])
}

RingBufferTests.test("TestReserveCapacity") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.isEmpty, false)
  expectEqual(ringBuffer.capacity, 3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.pushFront(4)
  expectEqual(ringBuffer.map { $0 }, [4, 1, 2])
  ringBuffer.reserveCapacity(2)
  ringBuffer.pushBack(5)
  ringBuffer.pushFront(6)
  expectEqual(ringBuffer.map { $0 }, [6, 4, 1, 2, 5])
  ringBuffer.pushBack(7)
  expectEqual(ringBuffer.map { $0 }, [4, 1, 2, 5, 7])
  ringBuffer.pushFront(8)
  expectEqual(ringBuffer.map { $0 }, [8, 4, 1, 2, 5])
}

RingBufferTests.test("TestRemoveLast") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.removeLast()
  expectEqual(ringBuffer.map { $0 }, [1, 2])
  ringBuffer.removeLast(2)
  expectEqual(ringBuffer.map { $0 }, [])
  expectEqual(ringBuffer.popLast(), nil)
  ringBuffer.pushFront(1)
  expectEqual(ringBuffer.popLast(), 1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [2, 3])
}

RingBufferTests.test("TestRemoveFirst") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.removeFirst()
  expectEqual(ringBuffer.map { $0 }, [2, 3])
  ringBuffer.removeFirst(2)
  expectEqual(ringBuffer.map { $0 }, [])
  expectEqual(ringBuffer.popFirst(), nil)
  ringBuffer.pushFront(1)
  expectEqual(ringBuffer.popFirst(), 1)
  ringBuffer.pushFront(2)
  ringBuffer.pushFront(3)
  expectEqual(ringBuffer.map { $0 }, [3, 2])
}

RingBufferTests.test("TestRemoveAllKeepingCapacityFalse") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.removeAll()
  ringBuffer.pushBack(4)
  expectEqual(ringBuffer.map { $0 }, [])
  expectEqual(ringBuffer.isEmpty, true)
  expectEqual(ringBuffer.isFull, true)
  expectEqual(ringBuffer.capacity, 0)
}

RingBufferTests.test("TestRemoveAllKeepingCapacityTrue") {
  var ringBuffer = RingBuffer<Int>(capacity: 3)
  ringBuffer.pushFront(1)
  ringBuffer.pushBack(2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [1, 2, 3])
  ringBuffer.removeAll(keepingCapacity: true)
  expectEqual(ringBuffer.isEmpty, true)
  expectEqual(ringBuffer.isFull, false)
  expectEqual(ringBuffer.capacity, 3)
  ringBuffer.pushBack(4)
  ringBuffer.pushBack(5)
  ringBuffer.pushFront(6)
  expectEqual(ringBuffer.map { $0 }, [6, 4, 5])
}

RingBufferTests.test("TestAppend") {
  var ringBuffer = RingBuffer<Int>(capacity: 0)
  expectEqual(ringBuffer.isEmpty, true)
  ringBuffer.append(1)
  expectEqual(ringBuffer.isEmpty, false)
  expectEqual(ringBuffer.capacity, 1)
  expectEqual(ringBuffer.map { $0 }, [1])
  ringBuffer.resize(newCapacity: 2)
  ringBuffer.append(2)
  expectEqual(ringBuffer.map { $0 }, [1, 2])
  expectEqual(ringBuffer.capacity, 2)
  ringBuffer.pushBack(3)
  expectEqual(ringBuffer.map { $0 }, [2, 3])
  ringBuffer.pushFront(4)
  expectEqual(ringBuffer.map { $0 }, [4, 2])
  ringBuffer.append(contentsOf: [1, 2, 3, 4])
  expectEqual(ringBuffer.capacity, 6)
  expectEqual(ringBuffer.map { $0 }, [4, 2, 1, 2, 3, 4])
  ringBuffer.pushFront(5)
  expectEqual(ringBuffer.map { $0 }, [5, 4, 2, 1, 2, 3])
  ringBuffer.pushBack(6)
  expectEqual(ringBuffer.map { $0 }, [4, 2, 1, 2, 3, 6])
}

runAllTests()