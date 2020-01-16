// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

let parent = -200..<200
let source: RangeSet = [1..<5, 8..<10, 20..<22, 27..<29]

let letterString = "ABCdefGHIjklMNOpqrStUvWxyz"
let lowercaseLetters = letterString.filter { $0.isLowercase }
let uppercaseLetters = letterString.filter { $0.isUppercase }

func buildRandomRangeSet(iterations: Int = 100) -> RangeSet<Int> {
  var set = RangeSet<Int>()
  for _ in 0..<100 {
    var (a, b) = (Int.random(in: -100...100), Int.random(in: -100...100))
    if (a > b) { swap(&a, &b) }
    if Double.random(in: 0..<1) > 0.3 {
      set.insert(a..<b)
    } else {
      set.remove(a..<b)
    }
  }
  return set
}

extension Collection {
  func every(_ n: Int) -> [Element] {
    sequence(first: startIndex) { i in
      self.index(i, offsetBy: n, limitedBy: self.endIndex)
    }.map { self[$0] }
  }
}

let RangeSetTests = TestSuite("RangeSet")

RangeSetTests.test("contains") {
  expectFalse(source.contains(0))
  expectTrue(source.contains(1))
  expectTrue(source.contains(4))
  expectFalse(source.contains(5))
  expectTrue(source.contains(28))
  expectFalse(source.contains(29))
  
  for _ in 0..<1000 {
    let set = buildRandomRangeSet()
    for i in parent.indices[set] {
      expectTrue(set.contains(i))
    }
    
    let inverted = set._inverted(within: parent)
    for i in parent.indices[inverted] {
      expectFalse(set.contains(i))
    }
  }
  
  expectTrue(source.contains(1..<5))
  expectTrue(source.contains(1..<4))
  expectTrue(source.contains(2..<5))
  expectFalse(source.contains(2..<6))
  expectFalse(source.contains(0..<5))
  expectFalse(source.contains(2..<9))
}

RangeSetTests.test("insertions") {
  do {
    // Overlap from middle to middle
    var s = source
    s.insert(3..<21)
    expectEqualSequence(s.ranges, [1..<22, 27..<29])
  }
  
  do {
    // insert in middle
    var s = source
    s.insert(13..<15)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 13..<15, 20..<22, 27..<29])
  }
  
  do {
    // extend a range
    var s = source
    s.insert(22..<25)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 20..<25, 27..<29])
  }
  
  do {
    // extend at beginning of range
    var s = source
    s.insert(17..<20)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 17..<22, 27..<29])
  }
  
  do {
    // insert at the beginning
    var s = source
    s.insert(-10 ..< -5)
    expectEqualSequence(s.ranges, [-10 ..< -5, 1..<5, 8..<10, 20..<22, 27..<29])
  }
  
  do {
    // insert at the end
    var s = source
    s.insert(35 ..< 40)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 20..<22, 27..<29, 35..<40])
  }
  
  do {
    // Overlap multiple ranges
    var s = source
    s.insert(0..<21)
    expectEqualSequence(s.ranges, [0..<22, 27..<29])
  }
  
  do {
    // Insert at end of range
    var s = source
    s.insert(22, within: parent)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 20..<23, 27..<29])
  }
  
  do {
    // Insert between ranges
    var s = source
    s.insert(14, within: parent)
    expectEqualSequence(s.ranges, [1..<5, 8..<10, 14..<15, 20..<22, 27..<29])
  }
}

RangeSetTests.test("removals") {
  var s = source
  s.remove(4..<28)
  expectEqualSequence(s.ranges, [1..<4, 28..<29])
  s.remove(3, within: parent)
  expectEqualSequence(s.ranges, [1..<3, 28..<29])
}

RangeSetTests.test("invariants") {
  for _ in 0..<1000 {
    let set = buildRandomRangeSet()
    
    // No empty ranges allowed
    expectTrue(set.ranges.allSatisfy { !$0.isEmpty })
    
    // No overlapping / out-of-order ranges allowed
    let adjacentRanges = zip(set.ranges, set.ranges.dropFirst())
    expectTrue(adjacentRanges.allSatisfy { $0.upperBound < $1.lowerBound })
  }
}

RangeSetTests.test("intersection") {
  func intersectionViaSet(_ s1: RangeSet<Int>, _ s2: RangeSet<Int>) -> RangeSet<Int> {
    let set1 = Set(parent.indices[s1])
    let set2 = Set(parent.indices[s2])
    return RangeSet(set1.intersection(set2), within: parent)
  }
  
  do {
    // Simple test
    let set1: RangeSet = [0..<5, 9..<14]
    let set2: RangeSet = [1..<3, 4..<6, 8..<12]
    let intersection: RangeSet = [1..<3, 4..<5, 9..<12]
    expectEqual(set1.intersection(set2), intersection)
    expectEqual(set2.intersection(set1), intersection)
  }
  
  do {
    // Test with upper bound / lower bound equality
    let set1: RangeSet = [10..<20, 30..<40]
    let set2: RangeSet = [15..<30, 40..<50]
    let intersection: RangeSet = [15..<20]
    expectEqual(set1.intersection(set2), intersection)
    expectEqual(set2.intersection(set1), intersection)
  }
  
  for _ in 0..<100 {
    let set1 = buildRandomRangeSet()
    let set2 = buildRandomRangeSet()
    
    let rangeSetIntersection = set1.intersection(set2)
    let stdlibSetIntersection = intersectionViaSet(set1, set2)
    expectEqual(rangeSetIntersection, stdlibSetIntersection)
  }
}

RangeSetTests.test("symmetricDifference") {
  func symmetricDifferenceViaSet(_ s1: RangeSet<Int>, _ s2: RangeSet<Int>) -> RangeSet<Int> {
    let set1 = Set(parent.indices[s1])
    let set2 = Set(parent.indices[s2])
    return RangeSet(set1.symmetricDifference(set2), within: parent)
  }
  
  do {
    // Simple test
    let set1: RangeSet = [0..<5, 9..<14]
    let set2: RangeSet = [1..<3, 4..<6, 8..<12]
    let difference: RangeSet = [0..<1, 3..<4, 5..<6, 8..<9, 12..<14]
    expectEqual(set1.symmetricDifference(set2), difference)
    expectEqual(set2.symmetricDifference(set1), difference)
  }
  
  do {
    // Test with upper bound / lower bound equality
    let set1: RangeSet = [10..<20, 30..<40]
    let set2: RangeSet = [15..<30, 40..<50]
    let difference: RangeSet = [10..<15, 20..<50]
    expectEqual(set1.symmetricDifference(set2), difference)
    expectEqual(set2.symmetricDifference(set1), difference)
  }
  
  for _ in 0..<100 {
    let set1 = buildRandomRangeSet()
    let set2 = buildRandomRangeSet()
    
    let rangeSetDifference = set1.symmetricDifference(set2)
    let stdlibSetDifference = symmetricDifferenceViaSet(set1, set2)
    expectEqual(rangeSetDifference, stdlibSetDifference)
  }
}

RangeSetTests.test("subranges(of:/where:)") {
  let a = [1, 2, 3, 4, 3, 3, 4, 5, 3, 4, 3, 3, 3]
  let indices = a.subranges(of: 3)
  expectEqual(indices, [2..<3, 4..<6, 8..<9, 10..<13])
  
  let allTheThrees = a[indices]
  expectEqual(allTheThrees.count, 7)
  expectTrue(allTheThrees.allSatisfy { $0 == 3 })
  expectEqual(Array(allTheThrees), Array(repeating: 3, count: 7))
  
  let lowerIndices = letterString.subranges(where: { $0.isLowercase })
  let lowerOnly = letterString[lowerIndices]
  expectEqualSequence(lowerOnly, lowercaseLetters)
  expectEqualSequence(lowerOnly.reversed(), lowercaseLetters.reversed())
  
  let upperOnly = letterString[lowerIndices._inverted(within: letterString)]
  expectEqualSequence(upperOnly, uppercaseLetters)
  expectEqualSequence(upperOnly.reversed(), uppercaseLetters.reversed())
}

RangeSetTests.test("removeAll") {
  var a = [1, 2, 3, 4, 3, 3, 4, 5, 3, 4, 3, 3, 3]
  let indices = a.subranges(of: 3)
  a.removeAll(in: indices)
  expectEqual(a, [1, 2, 4, 4, 5, 4])
  
  var numbers = Array(1...20)
  numbers.removeAll(in: [2..<5, 10..<15, 18..<20])
  expectEqual(numbers, [1, 2, 6, 7, 8, 9, 10, 16, 17, 18])
  
  var str = letterString
  let lowerIndices = str.subranges(where: { $0.isLowercase })
  
  let upperOnly = str.removingAll(in: lowerIndices)
  expectEqualSequence(upperOnly, uppercaseLetters)
  
  str.removeAll(in: lowerIndices)
  expectEqualSequence(str, uppercaseLetters)
}

RangeSetTests.test("shift/range") {
  let a = ["A", "B", "C", "D", "E", "F"]
  for lowerBound in a.indices {
    for upperBound in lowerBound..<a.endIndex {
      for destination in a.indices {
        var b = a
        let source = lowerBound..<upperBound
        let result = b.shift(from: source, to: destination)
        expectEqual(b[result], a[source])
        
        // Compare result with RangeSet-based move
        var c = a
        _ = c.gather(RangeSet(source), at: destination)
        expectEqualSequence(b, c)
        
        // Manual comparison
        if destination < source.lowerBound {
          let c = [
            a[..<destination],
            a[source],
            a[destination..<source.lowerBound],
            a[source.upperBound...],
            ].joined()
          expectEqualSequence(b, c)
        }
        else if destination >= source.upperBound {
          let c = [
            a[..<source.lowerBound],
            a[source.upperBound..<destination],
            a[source],
            a[destination...],
            ].joined()
          expectEqualSequence(b, c)
        }
        else {
          expectEqualSequence(b, a)
        }
      }
    }
  }
  
  // closed range
  var b = a
  expectEqual(b.shift(from: 2...3, to: 1), 1..<3)
  expectEqual(b, ["A", "C", "D", "B", "E", "F"])
}

RangeSetTests.test("shift/single") {
  let a = ["A", "B", "C", "D", "E", "F"]
  for source in a.indices {
    for dest in a.startIndex...a.endIndex {
      var b = a
      var c = a
      let rs = RangeSet(source, within: a)
      let resultingIndex = b.shift(from: source, to: dest)
      c.gather(rs, at: dest)
      expectEqual(a[source], b[resultingIndex])
      expectEqual(b, c)
    }
  }
}

RangeSetTests.test("gather/rangeset") {
  // Move before
  var numbers = Array(1...20)
  let range1 = numbers.gather([10..<15, 18..<20], at: 4)
  expectEqual(range1, 4..<11)
  expectEqual(numbers, [
    1, 2, 3, 4,
    11, 12, 13, 14, 15,
    19, 20,
    5, 6, 7, 8, 9, 10, 16, 17, 18])
  
  // Move to start
  numbers = Array(1...20)
  let range2 = numbers.gather([10..<15, 18..<20], at: 0)
  expectEqual(range2, 0..<7)
  expectEqual(numbers, [
    11, 12, 13, 14, 15,
    19, 20,
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 16, 17, 18])
  
  // Move to end
  numbers = Array(1...20)
  let range3 = numbers.gather([10..<15, 18..<20], at: 20)
  expectEqual(range3, 13..<20)
  expectEqual(numbers, [
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 16, 17, 18,
    11, 12, 13, 14, 15,
    19, 20,
  ])
  
  // Move to middle of selected elements
  numbers = Array(1...20)
  let range4 = numbers.gather([10..<15, 18..<20], at: 14)
  expectEqual(range4, 10..<17)
  expectEqual(numbers, [
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15,
    19, 20,
    16, 17, 18])
  
  // Move none
  numbers = Array(1...20)
  let range5 = numbers.gather([], at: 10)
  expectEqual(range5, 10..<10)
  expectEqual(numbers, Array(1...20))
}

RangeSetTests.test("gather/predicate") {
  for length in 0..<11 {
    let initial = Array(0..<length)
    
    for destination in 0..<length {
      for modulus in 1...5 {
        let f: (Int) -> Bool = { $0.isMultiple(of: modulus) }
        let notf = { !f($0) }
        
        var array = initial
        var range = array.gather(at: destination, where: f)
        expectEqualSequence(array[range], initial.filter(f))
        expectEqualSequence(
          array[..<range.lowerBound] + array[range.upperBound...],
          initial.filter(notf))
        
        array = initial
        range = array.gather(at: destination, where: notf)
        expectEqualSequence(array[range], initial.filter(notf))
        expectEqualSequence(
          array[..<range.lowerBound] + array[range.upperBound...],
          initial.filter(f))
      }
    }
  }
}

RangeSetTests.test("gather/noCOW") {
  let numbers = Array(1...20)
  expectNoCopyOnWrite(numbers) { numbers in
    numbers.gather([10..<15, 18..<20], at: 4)
  }
  expectNoCopyOnWrite(numbers) { numbers in
    numbers.removeAll(in: [2..<5, 10..<15, 18..<20])
  }
}

RangeSetTests.test("DiscontiguousSliceSlicing") {
  let initial = 1...100
  
  // Build an array of ranges that include alternating groups of 5 elements
  // e.g. 1...5, 11...15, etc
  let rangeStarts = initial.indices.every(10)
  let rangeEnds = rangeStarts.compactMap {
    initial.index($0, offsetBy: 5, limitedBy: initial.endIndex)
  }
  let ranges = zip(rangeStarts, rangeEnds).map(Range.init)
  
  // Create a collection of the elements represented by `ranges` without
  // using `RangeSet`
  let chosenElements = ranges.map { initial[$0] }.joined()
  
  let set = RangeSet(ranges)
  let discontiguousSlice = initial[set]
  expectEqualSequence(discontiguousSlice, chosenElements)
  
  for (chosenIdx, disIdx) in zip(chosenElements.indices, discontiguousSlice.indices) {
    expectEqualSequence(chosenElements[chosenIdx...], discontiguousSlice[disIdx...])
    expectEqualSequence(chosenElements[..<chosenIdx], discontiguousSlice[..<disIdx])
    for (chosenUpper, disUpper) in
      zip(chosenElements.indices[chosenIdx...], discontiguousSlice.indices[disIdx...])
    {
      expectEqualSequence(
        Array(chosenElements[chosenIdx..<chosenUpper]),
        Array(discontiguousSlice[disIdx..<disUpper]))
    }
  }
}

runAllTests()
