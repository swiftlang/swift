// RUN: %target-run-simple-swift
// REQUIRES: executable_test
import StdlibUnittest

let CircularBufferTests = TestSuite("CircularBuffer")

private func makeCircularBuffer<S: Sequence>(frontSequence: S, backSequence: S, capacity: Int) -> CircularBuffer<S.Element> {
  var circularBuffer = CircularBuffer<S.Element>(capacity: capacity)
  circularBuffer.pushFront(contentsOf: frontSequence)
  circularBuffer.pushBack(contentsOf: backSequence)

  return circularBuffer
}

// CircularBuffer init tests

CircularBufferTests.test("CapacityInit") {
  let circularBuffer = CircularBuffer<Int>(capacity: 3)
  expectEqual(circularBuffer, [])
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(circularBuffer.count, 0)
  expectEqual(circularBuffer.underestimatedCount, 0)
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.isFull, false)
}

CircularBufferTests.test("ArrayLiteralInit") {
  let circularBuffer: CircularBuffer<Int> = [1, 2, 3]
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(circularBuffer.count, 3)
  expectEqual(circularBuffer.underestimatedCount, 3)
  expectEqual(circularBuffer.isEmpty, false)
  expectEqual(circularBuffer.isFull, true)
}

CircularBufferTests.test("SequenceInit") {
  let array: ContiguousArray<Int> = [1, 2, 3]
  let circularBuffer: CircularBuffer<Int> = CircularBuffer<Int>(array)
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(circularBuffer.count, 3)
  expectEqual(circularBuffer.underestimatedCount, 3)
  expectEqual(circularBuffer.isEmpty, false)
  expectEqual(circularBuffer.isFull, true)
}

CircularBufferTests.test("RepatedInit") {
  let circularBuffer: CircularBuffer<Int> = CircularBuffer<Int>.init(repeating: 1, count: 3)
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(circularBuffer, [1, 1, 1])
  expectEqual(circularBuffer.count, 3)
  expectEqual(circularBuffer.underestimatedCount, 3)
  expectEqual(circularBuffer.isEmpty, false)
  expectEqual(circularBuffer.isFull, true)
}

CircularBufferTests.test("EmptyInit") {
  let circularBuffer = CircularBuffer<Int>()
  expectEqual(circularBuffer.capacity, 0)
  expectEqual(circularBuffer, [])
  expectEqual(circularBuffer.count, 0)
  expectEqual(circularBuffer.underestimatedCount, 0)
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.isFull, true)
}

// MARK: CircularBuffer push pop method tests

CircularBufferTests.test("PushBack") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)
  circularBuffer.pushBack(2)
  circularBuffer.pushBack(3)
  expectEqual(circularBuffer, [1, 2, 3])
  circularBuffer.pushBack(4)
  expectEqual(circularBuffer, [2, 3, 4])
  circularBuffer.pushBack(5)
  expectEqual(circularBuffer, [3, 4, 5])
  circularBuffer.pushBack(6)
  expectEqual(circularBuffer, [4, 5, 6])
  circularBuffer.pushBack(contentsOf: [7, 8, 9])
  expectEqual(circularBuffer, [7, 8, 9])
}

CircularBufferTests.test("PushBackCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)
  var copy = circularBuffer
  copy.pushBack(2)
  expectEqual(circularBuffer, [1])
  expectEqual(copy, [1, 2])
}

CircularBufferTests.test("PushBackSequenceCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)
  var copy = circularBuffer
  copy.pushBack(contentsOf: [2, 3])

  expectEqual(circularBuffer, [1])
  expectEqual(copy, [1, 2, 3])
}

CircularBufferTests.test("PopBack") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)
  expectEqual(circularBuffer.popBack(), 1)
  circularBuffer.pushBack(2)
  circularBuffer.pushBack(3)
  circularBuffer.pushBack(4)
  circularBuffer.pushBack(5)
  expectEqual(circularBuffer, [3, 4, 5])
  expectEqual(circularBuffer.popBack(), 5)
  expectEqual(circularBuffer, [3, 4])
  expectEqual(circularBuffer.popBack(), 4)
  expectEqual(circularBuffer.popBack(), 3)
  expectEqual(circularBuffer, [])
}

CircularBufferTests.test("PopBackCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)
  var copy = circularBuffer
  copy.popBack()

  expectEqual(circularBuffer, [1])
  expectEqual(copy, [])
}

CircularBufferTests.test("PushFront") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)
  circularBuffer.pushFront(2)
  circularBuffer.pushFront(3)
  expectEqual(circularBuffer, [3, 2, 1])
  circularBuffer.pushFront(4)
  expectEqual(circularBuffer, [4, 3, 2])
  circularBuffer.pushFront(5)
  expectEqual(circularBuffer, [5, 4, 3])
  circularBuffer.pushFront(6)
  expectEqual(circularBuffer, [6, 5, 4])
  circularBuffer.pushFront(contentsOf: [7, 8, 9])
  expectEqual(circularBuffer, [9, 8, 7])
}

CircularBufferTests.test("PushFrontCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)

  var copy = circularBuffer
  copy.pushFront(2)

  expectEqual(circularBuffer, [1])
  expectEqual(copy, [2, 1])
}

CircularBufferTests.test("PushFrontSequenceCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)

  var copy = circularBuffer
  copy.pushFront(contentsOf: [2, 3])

  expectEqual(circularBuffer, [1])
  expectEqual(copy, [3, 2, 1])
}

CircularBufferTests.test("PopFront") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)
  circularBuffer.pushFront(2)
  circularBuffer.pushFront(3)
  expectEqual(circularBuffer, [3, 2, 1])
  expectEqual(circularBuffer.popFront(), 3)
  expectEqual(circularBuffer, [2, 1])
  circularBuffer.pushFront(5)
  circularBuffer.pushFront(6)
  expectEqual(circularBuffer, [6, 5, 2])
  expectEqual(circularBuffer.popFront(), 6)
  expectEqual(circularBuffer.popFront(), 5)
  expectEqual(circularBuffer, [2])
  expectEqual(circularBuffer.popFront(), 2)
  expectEqual(circularBuffer, [])
}

CircularBufferTests.test("PopFrontCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)

  var copy = circularBuffer

  copy.popFront()

  expectEqual(circularBuffer, [1])
  expectEqual(copy, [])
}

CircularBufferTests.test("MakeCircularBuffer") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushFront(1)
  circularBuffer.pushFront(2)
  circularBuffer.pushBack(1)
  circularBuffer.pushBack(2)
  circularBuffer.pushBack(3)
  expectEqual(circularBuffer, makeCircularBuffer(frontSequence: [1, 2], backSequence: [1, 2, 3], capacity: 3))
}

// MARK: CircularBuffer capacity tests

CircularBufferTests.test("ResizeCapacityToZero") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.isFull, true)
  circularBuffer.resize(newCapacity: 0)
  expectEqual(circularBuffer.isFull, true)
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.map { $0 }, [])
  circularBuffer.pushFront(1)
  circularBuffer.pushBack(2)
  expectEqual(circularBuffer.isFull, true)
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.map { $0 }, [])
}

CircularBufferTests.test("ResizeCapacityEqual") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.resize(newCapacity: 3)
  expectEqual(circularBuffer.isFull, true)
  circularBuffer.pushBack(4)
  expectEqual(circularBuffer, [2, 3, 4])
}

CircularBufferTests.test("ResizeCapacityHigher") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.isFull, true)
  circularBuffer.pushBack(4)
  expectEqual(circularBuffer, [2, 3, 4])
  circularBuffer.resize(newCapacity: 5)
  expectEqual(circularBuffer.capacity, 5)
  circularBuffer.pushFront(5)
  circularBuffer.pushBack(6)
  expectEqual(circularBuffer, [5, 2, 3, 4, 6])
  expectEqual(circularBuffer.isFull, true)
  circularBuffer.pushBack(7)
  circularBuffer.pushBack(8)
  expectEqual(circularBuffer, [3, 4, 6, 7, 8])
}

CircularBufferTests.test("ResizeCapacityLower") {
  /// Case 1: Elements are between head and tail without wrapping at the end of array
  var firstCase = makeCircularBuffer(frontSequence: [], backSequence: [1, 2, 3], capacity: 3)
  expectEqual(firstCase.isFull, true)
  firstCase.resize(newCapacity: 2)
  expectEqual(firstCase.capacity, 2)
  expectEqual(firstCase.isFull, true)
  expectEqual(firstCase, [1, 2])
  /// Case 2: Elements after head greater or equal to elements to move
  var secondCase = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(secondCase.isFull, true)
  secondCase.resize(newCapacity: 1)
  expectEqual(secondCase.capacity, 1)
  expectEqual(secondCase.isFull, true)
  expectEqual(secondCase, [1])
  /// Case 3: Elements after head lower than elements to move
  var thirdCase = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(thirdCase.isFull, true)
  thirdCase.resize(newCapacity: 2)
  expectEqual(thirdCase.capacity, 2)
  expectEqual(thirdCase.isFull, true)
  expectEqual(thirdCase, [1, 2])
}

CircularBufferTests.test("ResizeCOW") {
  var circularBuffer = CircularBuffer<Int>(capacity: 3)
  circularBuffer.pushBack(1)

  var copy = circularBuffer

  copy.resize(newCapacity: 4)

  expectEqual(circularBuffer.capacity, 3)
  expectEqual(copy.capacity, 4)
  expectEqual(circularBuffer, [1])
  expectEqual(copy, [1])
}

// MARK: CircularBuffer RandomAccessCollection method tests

CircularBufferTests.test("RandomAccessCollectionMethods") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.startIndex, 0)
  expectEqual(circularBuffer.endIndex, 3)
  expectEqual(circularBuffer[0], 1)
  expectEqual(circularBuffer[1], 2)
  expectEqual(circularBuffer[2], 3)
  expectEqual(circularBuffer[0...2].map { $0 }, [1, 2, 3])
  circularBuffer.pushFront(4)
  expectEqual(circularBuffer[0...2].map { $0 }, [4, 1, 2])
  circularBuffer.pushFront(5)
  expectEqual(circularBuffer[0...2].map { $0  }, [5, 4, 1])
  expectEqual(circularBuffer.index(after: 0), 1)
  expectEqual(circularBuffer.index(before: 1), 0)
  expectEqual(circularBuffer.index(0, offsetBy: 1), 1)
  var index = 0
  circularBuffer.formIndex(after: &index)
  expectEqual(index, 1)
  circularBuffer.formIndex(before: &index)
  expectEqual(index, 0)
  circularBuffer.formIndex(&index, offsetBy: 1)
  expectEqual(index, 1)
}

CircularBufferTests.test("RandomAccessCollectionMethodsCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer[0], 1)

  var copy = circularBuffer
  copy[0] = 4

  expectEqual(circularBuffer[0], 1)
  expectEqual(copy[0], 4)
}

// MARK: CircularBuffer RangeReplaceableCollection method tests

CircularBufferTests.test("ReplaceSubrange") {
  /// Case 1: Elements count equal zero
  var firstCase: CircularBuffer<Int> = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  firstCase.replaceSubrange(0..<3, with: [])
  expectEqual(firstCase, [4, 5])
  firstCase.pushBack(6)
  expectEqual(firstCase, [4, 5, 6])
  firstCase.pushFront(7)
  expectEqual(firstCase, [7, 4, 5, 6])
  /// Case 2: Subrange count equal elements count
  var secondCase = makeCircularBuffer(frontSequence: [1, 2], backSequence: [3, 4, 5], capacity: 5)
  secondCase.replaceSubrange(0..<3, with: [3, 2, 1])
  expectEqual(secondCase, [3, 2, 1, 4, 5])
  secondCase.pushBack(6)
  expectEqual(secondCase, [2, 1, 4, 5, 6])
  secondCase.pushFront(7)
  expectEqual(secondCase, [7, 2, 1, 4, 5])
  /// Case 3: Subrange count greater than elements count
  var thirdCase: CircularBuffer<Int> = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  thirdCase.replaceSubrange(0..<4, with: [6, 7, 8])
  expectEqual(thirdCase, [6, 7, 8, 5])
  thirdCase.pushBack(9)
  expectEqual(thirdCase, [6, 7, 8, 5, 9])
  thirdCase.pushFront(10)
  expectEqual(thirdCase, [10, 6, 7, 8, 5])
  /// Case 4: Subrange count less than elements count with additional capacity
  var fourthCase = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3], capacity: 3)
  fourthCase.replaceSubrange(0..<1, with: [3, 4, 5])
  expectEqual(fourthCase, [3, 4, 5, 2, 3])
  fourthCase.pushBack(6)
  expectEqual(fourthCase, [4, 5, 2, 3, 6])
  fourthCase.pushFront(7)
  expectEqual(fourthCase, [7, 4, 5, 2, 3])
  /// Case 5: Subrange count less than elements count without additional capacity
  var fifthCase = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3], capacity: 5)
  fifthCase.replaceSubrange(0..<3, with: [1, 2, 3, 4, 5])
  expectEqual(fifthCase, [1, 2, 3, 4, 5])
  fifthCase.pushBack(6)
  expectEqual(fifthCase, [2, 3, 4, 5, 6])
  fifthCase.pushFront(7)
  expectEqual(fifthCase, [7, 2, 3, 4, 5])
}

CircularBufferTests.test("ReplaceSubrangeCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer
  copy.removeSubrange(1..<2)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1, 3])
}

CircularBufferTests.test("RemoveSubrange") {
  /// Case 1: Subrange starts with 0
  var firstCase = makeCircularBuffer(frontSequence: [1, 2], backSequence: [3, 4, 5], capacity: 5)
  firstCase.removeSubrange(0..<2)
  expectEqual(firstCase, [3, 4, 5])
  /// Case 2: Subrange ends with circular array end
  var secondCase = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  expectEqual(secondCase, [1, 2, 3, 4, 5])
  secondCase.removeSubrange(3..<5)
  expectEqual(secondCase, [1, 2, 3])
  /// Case 3: Subrange start and ends between head and tail without wrapping at the end of array
  var thirdCase = makeCircularBuffer(frontSequence: [], backSequence: [1, 2, 3, 4, 5], capacity: 5)
  thirdCase.removeSubrange(2..<3)
  expectEqual(thirdCase, [1, 2, 4, 5])
  ///  Case 4: Subrange start and ends between head and tail with wrapping at the end of array and head + lower bound greater than capacity
  var fourthCase = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  fourthCase.removeSubrange(3..<4)
  expectEqual(fourthCase, [1, 2, 3, 5])
  ///  Case 5: Subrange start and ends between head and tail with wrapping at the end of array and head + upper bound greater than capacity
  var fifthCase = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  fifthCase.removeSubrange(1..<3)
  expectEqual(fifthCase, [1, 4, 5])
}

CircularBufferTests.test("RemoveSubrangeEmpty") {
  var circularBuffer = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  circularBuffer.removeSubrange(4..<4)
  expectEqual(circularBuffer, [1, 2, 3, 4, 5])
}

CircularBufferTests.test("RemoveSubrangeFull") {
  var circularBuffer = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  circularBuffer.removeSubrange(0..<5)
  expectEqual(circularBuffer, [])
}

CircularBufferTests.test("RemoveSubrangeCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer[0], 1)

  var copy = circularBuffer
  copy.removeSubrange(1..<2)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1, 3])
}

CircularBufferTests.test("InsertSingleElementAtIndex") {
  var circularBuffer = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  circularBuffer.insert(6, at: 0)
  expectEqual(circularBuffer.capacity, 7)
  expectEqual(circularBuffer, [6, 1, 2, 3, 4, 5])
  circularBuffer.insert(7, at: 6)
  expectEqual(circularBuffer.capacity, 7)
  expectEqual(circularBuffer, [6, 1, 2, 3, 4, 5, 7])
  circularBuffer.insert(8, at: 3)
  expectEqual(circularBuffer.capacity, 10)
  expectEqual(circularBuffer, [6, 1, 2, 8, 3, 4, 5, 7])
}

CircularBufferTests.test("InsertSingleElementAtIndexCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer
  copy.insert(4, at: 0)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [4, 1, 2, 3])
}

CircularBufferTests.test("InsertMultipleElementsAtIndex") {
  var circularBuffer = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  circularBuffer.insert(contentsOf: [6, 7], at: 0)
  expectEqual(circularBuffer.capacity, 7)
  expectEqual(circularBuffer, [6, 7, 1, 2, 3, 4, 5])
  circularBuffer.insert(contentsOf: [7, 8], at: 7)
  expectEqual(circularBuffer.capacity, 10)
  expectEqual(circularBuffer, [6, 7, 1, 2, 3, 4, 5, 7, 8])
  circularBuffer.insert(contentsOf: [8, 9], at: 3)
  expectEqual(circularBuffer.capacity, 15)
  expectEqual(circularBuffer, [6, 7, 1, 8, 9, 2, 3, 4, 5, 7, 8])
}

CircularBufferTests.test("InsertMultipleElementsAtIndexCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer
  copy.insert(contentsOf: [4, 5], at: 0)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [4, 5, 1, 2, 3])
}

CircularBufferTests.test("RemoveAtIndex") {
  var circularBuffer = makeCircularBuffer(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
  circularBuffer.remove(at: 0)
  expectEqual(circularBuffer, [2, 3, 4, 5])
  circularBuffer.remove(at: 3)
  expectEqual(circularBuffer, [2, 3, 4])
  circularBuffer.remove(at: 1)
  expectEqual(circularBuffer, [2, 4])
}

CircularBufferTests.test("RemoveAtIndexCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer
  copy.remove(at: 1)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1, 3])
}


CircularBufferTests.test("ReserveCapacity") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.pushFront(4)
  expectEqual(circularBuffer, [4, 1, 2])
  circularBuffer.reserveCapacity(1)
  expectEqual(circularBuffer.capacity, 3)
  circularBuffer.reserveCapacity(5)
  circularBuffer.pushBack(5)
  circularBuffer.pushFront(6)
  expectEqual(circularBuffer, [6, 4, 1, 2, 5])
  circularBuffer.pushBack(7)
  expectEqual(circularBuffer, [4, 1, 2, 5, 7])
  circularBuffer.pushFront(8)
  expectEqual(circularBuffer, [8, 4, 1, 2, 5])
}

CircularBufferTests.test("ReserveCapacityCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.capacity, 3)

  var copy = circularBuffer
  expectEqual(copy.capacity, 3)

  copy.reserveCapacity(4)
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(copy.capacity, 4)
}

CircularBufferTests.test("Append") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [], capacity: 1)
  circularBuffer.popFront()
  expectEqual(circularBuffer.isEmpty, true)
  circularBuffer.append(1)
  circularBuffer.append(2)
  expectEqual(circularBuffer.isEmpty, false)
  expectEqual(circularBuffer.capacity, 2)
  expectEqual(circularBuffer, [1, 2])
  circularBuffer.pushBack(3)
  expectEqual(circularBuffer, [2, 3])
  circularBuffer.pushFront(4)
  expectEqual(circularBuffer, [4, 2])
  circularBuffer.append(contentsOf: [1, 2, 3, 4])
  expectEqual(circularBuffer.capacity, 6)
  expectEqual(circularBuffer, [4, 2, 1, 2, 3, 4])
  circularBuffer.pushFront(5)
  expectEqual(circularBuffer, [5, 4, 2, 1, 2, 3])
  circularBuffer.pushBack(6)
  expectEqual(circularBuffer, [4, 2, 1, 2, 3, 6])
}

CircularBufferTests.test("AppendCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.capacity, 3)

  var copy = circularBuffer
  expectEqual(copy.capacity, 3)

  copy.append(1)
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(copy.capacity, 4)
}

CircularBufferTests.test("AppendSequenceCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  expectEqual(circularBuffer.capacity, 3)

  var copy = circularBuffer
  expectEqual(copy.capacity, 3)

  copy.append(contentsOf: [1, 2])
  expectEqual(circularBuffer.capacity, 3)
  expectEqual(copy.capacity, 5)
}

CircularBufferTests.test("RemoveFromBack") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.removeLast()
  expectEqual(circularBuffer, [1, 2])
  circularBuffer.removeLast(2)
  expectEqual(circularBuffer, [])
  expectEqual(circularBuffer.popLast(), nil)
  circularBuffer.pushFront(1)
  expectEqual(circularBuffer.popLast(), 1)
  circularBuffer.pushBack(2)
  circularBuffer.pushFront(1)
  expectEqual(circularBuffer, [1, 2])
}

CircularBufferTests.test("RemoveLastCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.removeLast()

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1, 2])
}

CircularBufferTests.test("RemoveLastKCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.removeLast(2)

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1])
}

CircularBufferTests.test("PopLastCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.popLast()

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1, 2])
}

CircularBufferTests.test("RemoveFromFront") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  circularBuffer.removeFirst()
  expectEqual(circularBuffer, [2, 3])
  circularBuffer.removeFirst(2)
  expectEqual(circularBuffer, [])
  expectEqual(circularBuffer.popFirst(), nil)
  circularBuffer.pushFront(1)
  expectEqual(circularBuffer.popFirst(), 1)
  circularBuffer.pushFront(2)
  circularBuffer.pushFront(3)
  expectEqual(circularBuffer, [3, 2])
}

CircularBufferTests.test("RemoveFirstCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.removeFirst()

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [2, 3])
}

CircularBufferTests.test("PopFirstCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.popFirst()

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [2, 3])
}

CircularBufferTests.test("RemoveAllKeepingCapacityFalse") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.removeAll()
  circularBuffer.pushBack(4)
  expectEqual(circularBuffer, [])
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.isFull, true)
  expectEqual(circularBuffer.capacity, 0)
}

CircularBufferTests.test("RemoveAllKeepingCapacityTrue") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.removeAll(keepingCapacity: true)
  expectEqual(circularBuffer.isEmpty, true)
  expectEqual(circularBuffer.isFull, false)
  expectEqual(circularBuffer.capacity, 3)
  circularBuffer.pushBack(4)
  circularBuffer.pushBack(5)
  circularBuffer.pushFront(6)
  expectEqual(circularBuffer, [6, 4, 5])
}

CircularBufferTests.test("RemoveAllCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.removeAll()

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [])
}

CircularBufferTests.test("RemoveAllWhere") {
  var circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)
  circularBuffer.removeAll(where: { $0 > 1})

  expectEqual(circularBuffer.count, 1)
  expectEqual(circularBuffer, [1])
}

CircularBufferTests.test("RemoveAllWhereCOW") {
  let circularBuffer = makeCircularBuffer(frontSequence: [1], backSequence: [2, 3], capacity: 3)

  var copy = circularBuffer

  copy.removeAll(where: { $0 > 1})

  expectEqual(circularBuffer, [1, 2, 3])
  expectEqual(copy, [1])
}

// MARK: CircularBuffer CustomStringConvertible method tests

CircularBufferTests.test("CustomStringConvertible") {
  expectEqual("[1, 2, 3]", CircularBuffer([1, 2, 3]).description)
  expectEqual("[\"1\", \"2\", \"3\"]", CircularBuffer(["1", "2", "3"]).description)
}

CircularBufferTests.test("CustomDebugStringConvertible") {
  expectEqual("CircularBuffer([1, 2, 3])", CircularBuffer([1, 2, 3]).debugDescription)
  expectEqual("CircularBuffer([\"1\", \"2\", \"3\"])", CircularBuffer(["1", "2", "3"]).debugDescription)
}

// MARK: CircularBuffer Equatable test

CircularBufferTests.test("Equatable") {
  expectEqual(CircularBuffer<Int>([1, 2, 3]) == CircularBuffer<Int>([1, 2, 3]), true)
  expectEqual(CircularBuffer<Int>([1, 2, 3]) == CircularBuffer<Int>([1, 2]), false)
  expectEqual(CircularBuffer<Int>([1, 2, 3]) != CircularBuffer<Int>([1, 2]), true)
}

runAllTests()