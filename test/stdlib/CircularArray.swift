// RUN: %target-run-simple-swift
// REQUIRES: executable_test
import StdlibUnittest

let CircularArrayTests = TestSuite("CircularArray")

private func makeCircularArray<S: Sequence>(frontSequence: S, backSequence: S, capacity: Int) -> CircularArray<S.Element> {
    var circularArray = CircularArray<S.Element>(capacity: capacity)
    circularArray.pushFront(contentsOf: frontSequence)
    circularArray.pushBack(contentsOf: backSequence)

    return circularArray
}

// CircularArray init tests

CircularArrayTests.test("CapacityInit") {
        let circularArray = CircularArray<Int>(capacity: 3)
        expectEqual(circularArray, [])
        expectEqual(circularArray.capacity, 3)
        expectEqual(circularArray.count, 0)
        expectEqual(circularArray.underestimatedCount, 0)
        expectEqual(circularArray.isEmpty, true)
        expectEqual(circularArray.isFull, false)
}

CircularArrayTests.test("ArrayLiteralInit") {
  let circularArray: CircularArray<Int> = [1, 2, 3]
  expectEqual(circularArray.capacity, 3)
  expectEqual(circularArray, [1, 2, 3])
  expectEqual(circularArray.count, 3)
  expectEqual(circularArray.underestimatedCount, 3)
  expectEqual(circularArray.isEmpty, false)
  expectEqual(circularArray.isFull, true)
}

CircularArrayTests.test("SequenceInit") {
  let array: ContiguousArray<Int> = [1, 2, 3]
  let circularArray: CircularArray<Int> = CircularArray<Int>(array)
  expectEqual(circularArray.capacity, 3)
  expectEqual(circularArray, [1, 2, 3])
  expectEqual(circularArray.count, 3)
  expectEqual(circularArray.underestimatedCount, 3)
  expectEqual(circularArray.isEmpty, false)
  expectEqual(circularArray.isFull, true)
}

CircularArrayTests.test("RepatedInit") {
  let circularArray: CircularArray<Int> = CircularArray<Int>(repeating: 1, count: 3)
  expectEqual(circularArray.capacity, 3)
  expectEqual(circularArray, [1, 1, 1])
  expectEqual(circularArray.count, 3)
  expectEqual(circularArray.underestimatedCount, 3)
  expectEqual(circularArray.isEmpty, false)
  expectEqual(circularArray.isFull, true)
}

CircularArrayTests.test("EmptyInit") {
  let circularArray = CircularArray<Int>()
  expectEqual(circularArray.capacity, 0)
  expectEqual(circularArray, [])
  expectEqual(circularArray.count, 0)
  expectEqual(circularArray.underestimatedCount, 0)
  expectEqual(circularArray.isEmpty, true)
  expectEqual(circularArray.isFull, true)
}

// CircularArray push pop method tests

CircularArrayTests.test("PushBack") {
  var circularArray = CircularArray<Int>(capacity: 3)
  circularArray.pushBack(1)
  circularArray.pushBack(2)
  circularArray.pushBack(3)
  expectEqual(circularArray, [1, 2, 3])
  circularArray.pushBack(4)
  expectEqual(circularArray, [2, 3, 4])
  circularArray.pushBack(5)
  expectEqual(circularArray, [3, 4, 5])
  circularArray.pushBack(6)
  expectEqual(circularArray, [4, 5, 6])
  circularArray.pushBack(contentsOf: [7, 8, 9])
  expectEqual(circularArray, [7, 8, 9])
}

CircularArrayTests.test("PushBackCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushBack(1)
    
    var copy = circularArray
    copy.pushBack(2)
    expectEqual(circularArray, [1])
    expectEqual(copy, [1, 2])
}

CircularArrayTests.test("PushBackSequenceCOW") {
  var circularArray = CircularArray<Int>(capacity: 3)
  circularArray.pushBack(1)
  
  var copy = circularArray
  copy.pushBack(contentsOf: [2, 3])
  expectEqual(circularArray, [1])
  expectEqual(copy, [1, 2, 3])
}

CircularArrayTests.test("PopBack") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushBack(1)
    expectEqual(circularArray.popBack(), 1)
    circularArray.pushBack(2)
    circularArray.pushBack(3)
    circularArray.pushBack(4)
    circularArray.pushBack(5)
    expectEqual(circularArray, [3, 4, 5])
    expectEqual(circularArray.popBack(), 5)
    expectEqual(circularArray, [3, 4])
    expectEqual(circularArray.popBack(), 4)
    expectEqual(circularArray.popBack(), 3)
    expectEqual(circularArray, [])
}

CircularArrayTests.test("PopBackCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushBack(1)
    
    var copy = circularArray
    copy.popBack()
    expectEqual(circularArray, [1])
    expectEqual(copy, [])
}

CircularArrayTests.test("PushFront") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)
    circularArray.pushFront(2)
    circularArray.pushFront(3)
    expectEqual(circularArray, [3, 2, 1])
    circularArray.pushFront(4)
    expectEqual(circularArray, [4, 3, 2])
    circularArray.pushFront(5)
    expectEqual(circularArray, [5, 4, 3])
    circularArray.pushFront(6)
    expectEqual(circularArray, [6, 5, 4])
    circularArray.pushFront(contentsOf: [7, 8, 9])
    expectEqual(circularArray, [9, 8, 7])
}

CircularArrayTests.test("PushFrontCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)

    var copy = circularArray
    copy.pushFront(2)
    expectEqual(circularArray, [1])
    expectEqual(copy, [2, 1])
}

CircularArrayTests.test("PushFrontSequenceCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)

    var copy = circularArray
    copy.pushFront(contentsOf: [2, 3])
    expectEqual(circularArray, [1])
    expectEqual(copy, [3, 2, 1])
}

CircularArrayTests.test("PopFront") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)
    circularArray.pushFront(2)
    circularArray.pushFront(3)
    expectEqual(circularArray, [3, 2, 1])
    expectEqual(circularArray.popFront(), 3)
    expectEqual(circularArray, [2, 1])
    circularArray.pushFront(5)
    circularArray.pushFront(6)
    expectEqual(circularArray, [6, 5, 2])
    expectEqual(circularArray.popFront(), 6)
    expectEqual(circularArray.popFront(), 5)
    expectEqual(circularArray, [2])
    expectEqual(circularArray.popFront(), 2)
    expectEqual(circularArray, [])
}

CircularArrayTests.test("PopFrontCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)

    var copy = circularArray
    copy.popFront()
    expectEqual(circularArray, [1])
    expectEqual(copy, [])
}

CircularArrayTests.test("MakeCircularArray") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushFront(1)
    circularArray.pushFront(2)
    circularArray.pushBack(1)
    circularArray.pushBack(2)
    circularArray.pushBack(3)
    expectEqual(circularArray, makeCircularArray(frontSequence: [1, 2], backSequence: [1, 2, 3], capacity: 3))
}

// CircularArray capacity tests

CircularArrayTests.test("ResizeCapacityToZero") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray.isFull, true)
    circularArray.resize(newCapacity: 0)
    expectEqual(circularArray.isFull, true)
    expectEqual(circularArray.isEmpty, true)
    expectEqual(circularArray.map { $0 }, [])
    circularArray.pushFront(1)
    circularArray.pushBack(2)
    expectEqual(circularArray.isFull, true)
    expectEqual(circularArray.isEmpty, true)
    expectEqual(circularArray.map { $0 }, [])
}

CircularArrayTests.test("ResizeCapacityEqual") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.resize(newCapacity: 3)
    expectEqual(circularArray.isFull, true)
    circularArray.pushBack(4)
    expectEqual(circularArray, [2, 3, 4])
}

CircularArrayTests.test("ResizeCapacityHigher") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray.isFull, true)
    circularArray.pushBack(4)
    expectEqual(circularArray, [2, 3, 4])
    circularArray.resize(newCapacity: 5)
    expectEqual(circularArray.capacity, 5)
    circularArray.pushFront(5)
    circularArray.pushBack(6)
    expectEqual(circularArray, [5, 2, 3, 4, 6])
    expectEqual(circularArray.isFull, true)
    circularArray.pushBack(7)
    circularArray.pushBack(8)
    expectEqual(circularArray, [3, 4, 6, 7, 8])
}

CircularArrayTests.test("ResizeCapacityLower") {
    /// Case 1: Elements are between head and tail without wrapping at the end of array
    var firstCase = makeCircularArray(frontSequence: [], backSequence: [1, 2, 3], capacity: 3)
    expectEqual(firstCase.isFull, true)
    firstCase.resize(newCapacity: 2)
    expectEqual(firstCase.capacity, 2)
    expectEqual(firstCase.isFull, true)
    expectEqual(firstCase, [1, 2])
    /// Case 2: Elements after head greater or equal to elements to move
    var secondCase = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(secondCase.isFull, true)
    secondCase.resize(newCapacity: 1)
    expectEqual(secondCase.capacity, 1)
    expectEqual(secondCase.isFull, true)
    expectEqual(secondCase, [1])
    /// Case 3: Elements after head lower than elements to move
    var thirdCase = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(thirdCase.isFull, true)
    thirdCase.resize(newCapacity: 2)
    expectEqual(thirdCase.capacity, 2)
    expectEqual(thirdCase.isFull, true)
    expectEqual(thirdCase, [1, 2])
}

CircularArrayTests.test("ResizeCOW") {
    var circularArray = CircularArray<Int>(capacity: 3)
    circularArray.pushBack(1)

    var copy = circularArray
    copy.resize(newCapacity: 4)
    expectEqual(circularArray.capacity, 3)
    expectEqual(copy.capacity, 4)
    expectEqual(circularArray, [1])
    expectEqual(copy, [1])
}

// MARK: CircularArray RandomAccessCollection method test
CircularArrayTests.test("RandomAccessCollectionMethods") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray.startIndex, 0)
    expectEqual(circularArray.endIndex, 3)
    expectEqual(circularArray[0], 1)
    expectEqual(circularArray[1], 2)
    expectEqual(circularArray[2], 3)
    expectEqual(circularArray[0...2].map { $0 }, [1, 2, 3])
    circularArray.pushFront(4)
    expectEqual(circularArray[0...2].map { $0 }, [4, 1, 2])
    circularArray.pushFront(5)
    expectEqual(circularArray[0...2].map { $0  }, [5, 4, 1])
}

CircularArrayTests.test("RandomAccessCollectionMethodsCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray[0], 1)

    var copy = circularArray
    copy[0] = 4
    expectEqual(circularArray[0], 1)
    expectEqual(copy[0], 4)
}

// CircularArray RangeReplaceableCollection method tests

CircularArrayTests.test("RemoveSubrange") {
    /// Case 1: Subrange starts with 0
    var firstCase = makeCircularArray(frontSequence: [1, 2], backSequence: [3, 4, 5], capacity: 5)
    firstCase.removeSubrange(0..<2)
    expectEqual(firstCase, [3, 4, 5])
    /// Case 2: Subrange ends with circular array end
    var secondCase = makeCircularArray(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
    expectEqual(secondCase, [1, 2, 3, 4, 5])
    secondCase.removeSubrange(3..<5)
    expectEqual(secondCase, [1, 2, 3])
    /// Case 3: Subrange start and ends between head and tail without wrapping at the end of array
    var thirdCase = makeCircularArray(frontSequence: [], backSequence: [1, 2, 3, 4, 5], capacity: 5)
    thirdCase.removeSubrange(2..<3)
    expectEqual(thirdCase, [1, 2, 4, 5])
    ///  Case 4: Subrange start and ends between head and tail with wrapping at the end of array and head + lower bound greater than capacity
    var fourthCase = makeCircularArray(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
    fourthCase.removeSubrange(3..<4)
    expectEqual(fourthCase, [1, 2, 3, 5])
    ///  Case 5: Subrange start and ends between head and tail with wrapping at the end of array and head + upper bound greater than capacity
    var fifthCase = makeCircularArray(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
    fifthCase.removeSubrange(1..<3)
    expectEqual(fifthCase, [1, 4, 5])
}

CircularArrayTests.test("RemoveSubrangeEmpty") {
    var circularArray = makeCircularArray(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
    circularArray.removeSubrange(4..<4)
    expectEqual(circularArray, [1, 2, 3, 4, 5])
}

CircularArrayTests.test("RemoveSubrangeFull") {
    var circularArray = makeCircularArray(frontSequence: [2, 1], backSequence: [3, 4, 5], capacity: 5)
    circularArray.removeSubrange(0..<5)
    expectEqual(circularArray, [])
}

CircularArrayTests.test("RemoveSubrangeCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)

    expectEqual(circularArray[0], 1)
    var copy = circularArray
    copy.removeSubrange(1..<2)
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [1, 3])
}

CircularArrayTests.test("ReserveCapacity") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.pushFront(4)
    expectEqual(circularArray, [4, 1, 2])
    circularArray.reserveCapacity(2)
    circularArray.pushBack(5)
    circularArray.pushFront(6)
    expectEqual(circularArray, [6, 4, 1, 2, 5])
    circularArray.pushBack(7)
    expectEqual(circularArray, [4, 1, 2, 5, 7])
    circularArray.pushFront(8)
    expectEqual(circularArray, [8, 4, 1, 2, 5])
}

CircularArrayTests.test("ReserveCapacityCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray.capacity, 3)

    var copy = circularArray
    expectEqual(copy.capacity, 3)
    copy.reserveCapacity(1)
    expectEqual(circularArray.capacity, 3)
    expectEqual(copy.capacity, 4)
}

CircularArrayTests.test("Append") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [], capacity: 1)
    circularArray.popFront()
    expectEqual(circularArray.isEmpty, true)
    circularArray.append(1)
    circularArray.append(2)
    expectEqual(circularArray.isEmpty, false)
    expectEqual(circularArray.capacity, 2)
    expectEqual(circularArray, [1, 2])
    circularArray.pushBack(3)
    expectEqual(circularArray, [2, 3])
    circularArray.pushFront(4)
    expectEqual(circularArray, [4, 2])
    circularArray.append(contentsOf: [1, 2, 3, 4])
    expectEqual(circularArray.capacity, 6)
    expectEqual(circularArray, [4, 2, 1, 2, 3, 4])
    circularArray.pushFront(5)
    expectEqual(circularArray, [5, 4, 2, 1, 2, 3])
    circularArray.pushBack(6)
    expectEqual(circularArray, [4, 2, 1, 2, 3, 6])
}

CircularArrayTests.test("AppendCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    
    expectEqual(circularArray.capacity, 3)
    var copy = circularArray
    
    expectEqual(copy.capacity, 3)
    copy.append(1)
    expectEqual(circularArray.capacity, 3)
    expectEqual(copy.capacity, 4)
}

CircularArrayTests.test("AppendSequenceCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    expectEqual(circularArray.capacity, 3)

    var copy = circularArray
    expectEqual(copy.capacity, 3)
    copy.append(contentsOf: [1, 2])
    expectEqual(circularArray.capacity, 3)
    expectEqual(copy.capacity, 5)
}

CircularArrayTests.test("RemoveFromBack") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.removeLast()
    expectEqual(circularArray, [1, 2])
    circularArray.removeLast(2)
    expectEqual(circularArray, [])
    expectEqual(circularArray.popLast(), nil)
    circularArray.pushFront(1)
    expectEqual(circularArray.popLast(), 1)
    circularArray.pushBack(2)
    circularArray.pushFront(1)
    expectEqual(circularArray, [1, 2])
}

CircularArrayTests.test("RemoveLastCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)

    var copy = circularArray
    copy.removeLast()
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [1, 2])
}

CircularArrayTests.test("RemoveLastKCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)

    var copy = circularArray
    copy.removeLast(2)
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [1])
}

CircularArrayTests.test("PopLastCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)

    var copy = circularArray
    copy.popLast()
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [1, 2])
}

CircularArrayTests.test("RemoveFromFront") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.removeFirst()
    expectEqual(circularArray, [2, 3])
    circularArray.removeFirst(2)
    expectEqual(circularArray, [])
    expectEqual(circularArray.popFirst(), nil)
    circularArray.pushFront(1)
    expectEqual(circularArray.popFirst(), 1)
    circularArray.pushFront(2)
    circularArray.pushFront(3)
    expectEqual(circularArray, [3, 2])
}

CircularArrayTests.test("RemoveFirstCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    
    var copy = circularArray
    copy.removeFirst()
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [2, 3])
}

CircularArrayTests.test("PopFirstCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    var copy = circularArray
    
    copy.popFirst()
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [2, 3])
}

CircularArrayTests.test("RemoveAllKeepingCapacityFalse") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.removeAll()
    circularArray.pushBack(4)
    expectEqual(circularArray, [])
    expectEqual(circularArray.isEmpty, true)
    expectEqual(circularArray.isFull, true)
    expectEqual(circularArray.capacity, 0)
}

CircularArrayTests.test("RemoveAllKeepingCapacityTrue") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.removeAll(keepingCapacity: true)
    expectEqual(circularArray.isEmpty, true)
    expectEqual(circularArray.isFull, false)
    expectEqual(circularArray.capacity, 3)
    circularArray.pushBack(4)
    circularArray.pushBack(5)
    circularArray.pushFront(6)
    expectEqual(circularArray, [6, 4, 5])
}

CircularArrayTests.test("RemoveAllCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    
    var copy = circularArray
    copy.removeAll()
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [])
}

CircularArrayTests.test("RemoveAllWhere") {
    var circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    circularArray.removeAll(where: { $0 > 1})
    expectEqual(circularArray.count, 1)
    expectEqual(circularArray, [1])
}

CircularArrayTests.test("RemoveAllWhereCOW") {
    let circularArray = makeCircularArray(frontSequence: [1], backSequence: [2, 3], capacity: 3)
    
    var copy = circularArray
    copy.removeAll(where: { $0 > 1})
    expectEqual(circularArray, [1, 2, 3])
    expectEqual(copy, [1])
}

runAllTests()