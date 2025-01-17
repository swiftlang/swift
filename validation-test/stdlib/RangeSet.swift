// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

@available(SwiftStdlib 6.0, *)
extension RangeSet: ExpressibleByArrayLiteral {
  public init(arrayLiteral elements: Range<Bound>...) {
    self.init(elements)
  }
}

extension Collection {
  func every(_ n: Int) -> [Element] {
    sequence(first: startIndex) { i in
      self.index(i, offsetBy: n, limitedBy: self.endIndex)
    }.map { self[$0] }
  }
}

let RangeSetTests = TestSuite("RangeSet")

if #available(SwiftStdlib 6.0, *) {
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
        set.insert(contentsOf: a..<b)
      } else {
        set.remove(contentsOf: a..<b)
      }
    }
    return set
  }
  
  RangeSetTests.test("initialization") {
    // Test coalescing and elimination of empty ranges
    do {
      let empty = RangeSet(Array(repeating: 0..<0, count: 100))
      expectTrue(empty.isEmpty)

      let repeated = RangeSet(Array(repeating: 0..<3, count: 100))
      expectEqual(repeated, [0..<3])
      
      let singleAfterEmpty = RangeSet(Array(repeating: 0..<0, count: 100) + [0..<3])
      expectEqual(singleAfterEmpty, [0..<3])

      let contiguousRanges = (0..<100).map { $0 ..< ($0 + 1) }
      expectEqual(RangeSet(contiguousRanges), [0..<100])
      expectEqual(RangeSet(contiguousRanges.shuffled()), [0..<100])
    }
    
    // The `buildRandomRangeSet()` function builds a range set via additions
    // and removals. This function creates an array of potentially empty or
    // overlapping ranges that can be used to initialize a range set.
    func randomRanges() -> [Range<Int>] {
      (0..<100).map { _ in
        let low = Int.random(in: 0...100)
        let count = Int.random(in: 0...20)
        return low ..< (low + count)
      }
    }
    
    for _ in 0..<1000 {
      let ranges = randomRanges()
      let set = RangeSet(ranges)
      
      // Manually construct a range set for comparison
      var comparison = RangeSet<Int>()
      for r in ranges {
        comparison.insert(contentsOf: r)
      }
      expectEqual(set, comparison)
    }
  }

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
      
      for i in parent.indices.removingSubranges(set) {
        expectFalse(set.contains(i))
      }
    }
  }

  RangeSetTests.test("insertions") {
    do {
      // Overlap from middle to middle
      var s = source
      s.insert(contentsOf: 3..<21)
      expectEqualSequence(s.ranges, [1..<22, 27..<29])
    }
    
    do {
      // insert in middle
      var s = source
      s.insert(contentsOf: 13..<15)
      expectEqualSequence(s.ranges, [1..<5, 8..<10, 13..<15, 20..<22, 27..<29])
    }
    
    do {
      // extend a range
      var s = source
      s.insert(contentsOf: 22..<25)
      expectEqualSequence(s.ranges, [1..<5, 8..<10, 20..<25, 27..<29])
    }
    
    do {
      // extend at beginning of range
      var s = source
      s.insert(contentsOf: 17..<20)
      expectEqualSequence(s.ranges, [1..<5, 8..<10, 17..<22, 27..<29])
    }
    
    do {
      // insert at the beginning
      var s = source
      s.insert(contentsOf: -10 ..< -5)
      expectEqualSequence(
        s.ranges,
        [-10 ..< -5, 1..<5, 8..<10, 20..<22, 27..<29]
      )
    }
    
    do {
      // insert at the end
      var s = source
      s.insert(contentsOf: 35 ..< 40)
      expectEqualSequence(s.ranges, [1..<5, 8..<10, 20..<22, 27..<29, 35..<40])
    }
    
    do {
      // Overlap multiple ranges
      var s = source
      s.insert(contentsOf: 0..<21)
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
    do {
      var s = source
      s.remove(contentsOf: 4..<28)
      expectEqualSequence(s.ranges, [1..<4, 28..<29])
      s.remove(3, within: parent)
      expectEqualSequence(s.ranges, [1..<3, 28..<29])
    }

    do {
      var s = source
      s.remove(contentsOf: 5..<7)
      expectEqualSequence(s.ranges, source.ranges)
      s.remove(5, within: parent)
      expectEqualSequence(s.ranges, source.ranges)
      s.remove(contentsOf: 5..<9)
      expectEqualSequence(s.ranges, [1..<5, 9..<10, 20..<22, 27..<29])
    }

    do {
      var s = source
      s.remove(contentsOf: 6..<8)
      expectEqualSequence(s.ranges, source.ranges)
      s.remove(7, within: parent)
      expectEqualSequence(s.ranges, source.ranges)
      s.remove(contentsOf: 4..<8)
      expectEqualSequence(s.ranges, [1..<4, 8..<10, 20..<22, 27..<29])
    }
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

  RangeSetTests.test("union") {
    func unionViaSet(
      _ s1: RangeSet<Int>,
      _ s2: RangeSet<Int>
    ) -> RangeSet<Int> {
      let set1 = Set(parent.indices[s1])
      let set2 = Set(parent.indices[s2])
      return RangeSet(set1.union(set2), within: parent)
    }

    func testUnion(
      _ set1: RangeSet<Int>,
      _ set2: RangeSet<Int>,
      expect union: RangeSet<Int>
    ) {
      expectEqual(set1.union(set2), union)
      expectEqual(set2.union(set1), union)

      var set3 = set1
      set3.formUnion(set2)
      expectEqual(set3, union)

      set3 = set2
      set3.formUnion(set1)
      expectEqual(set3, union)
    }
    
    // Simple tests
    testUnion([0..<5, 9..<14],
              [1..<3, 4..<6, 8..<12],
              expect: [0..<6, 8..<14])
    
    testUnion([10..<20, 50..<60],
              [15..<55, 58..<65],
              expect: [10..<65])

    // Test with upper bound / lower bound equality
    testUnion([10..<20, 30..<40],
              [15..<30, 40..<50],
              expect: [10..<50])
    
    for _ in 0..<100 {
      let set1 = buildRandomRangeSet()
      let set2 = buildRandomRangeSet()
      testUnion(set1, set2,
                expect: unionViaSet(set1, set2))
    }
  }
  
  RangeSetTests.test("intersection") {
    func intersectionViaSet(
      _ s1: RangeSet<Int>,
      _ s2: RangeSet<Int>
    ) -> RangeSet<Int> {
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
    func symmetricDifferenceViaSet(
      _ s1: RangeSet<Int>,
      _ s2: RangeSet<Int>
    ) -> RangeSet<Int> {
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

  RangeSetTests.test("isDisjoint") {
    func isDisjointViaSet(_ s1: RangeSet<Int>, _ s2: RangeSet<Int>) -> Bool {
      let set1 = Set(parent.indices[s1])
      let set2 = Set(parent.indices[s2])
      return set1.isDisjoint(with: set2)
    }

    do {
      // Simple test
      let set1: RangeSet = [0..<5, 9..<14]
      let set2: RangeSet = [1..<3, 4..<6, 8..<12]
      let set3: RangeSet = [6..<9, 14..<20]
      expectFalse(set1.isDisjoint(set2))
      expectFalse(set2.isDisjoint(set1))
      expectTrue(set1.isDisjoint(set3))
      expectTrue(set3.isDisjoint(set1))
    }
    
    for _ in 0..<100 {
      let set1 = buildRandomRangeSet()
      let set2 = buildRandomRangeSet()
      
      let rangeSetDisjoint = set1.isDisjoint(set2)
      let stdlibSetDisjoint = isDisjointViaSet(set1, set2)
      expectEqual(rangeSetDisjoint, stdlibSetDisjoint)
    }
  }

  RangeSetTests.test("isSubset") {
    func isSubsetViaSet(_ s1: RangeSet<Int>, _ s2: RangeSet<Int>) -> Bool {
      let set1 = Set(parent.indices[s1])
      let set2 = Set(parent.indices[s2])
      return set1.isSubset(of: set2)
    }

    do {
      // Simple test
      let set1: RangeSet = [0..<5, 9..<14]
      let set2: RangeSet = [1..<3, 4..<6, 8..<12]
      let set3: RangeSet = [6..<9, 14..<20]
      let set4: RangeSet = [1..<2, 10..<12]
      expectFalse(set1.isSubset(of: set2))
      expectFalse(set2.isSubset(of: set1))
      expectFalse(set1.isSubset(of: set3))
      expectFalse(set3.isSubset(of: set1))
      expectFalse(set1.isSubset(of: set4))
      expectFalse(set2.isSubset(of: set4))
      expectTrue(set4.isSubset(of: set1))
      expectTrue(set4.isSubset(of: set2))
    }
    
    for _ in 0..<100 {
      let set1 = buildRandomRangeSet()
      let set2 = buildRandomRangeSet()
      
      let rangeSetSubset = set1.isSubset(of: set2)
      let stdlibSetSubset = isSubsetViaSet(set1, set2)
      expectEqual(rangeSetSubset, stdlibSetSubset)
    }
  }

  RangeSetTests.test("indices(of:/where:)") {
    let a = [1, 2, 3, 4, 3, 3, 4, 5, 3, 4, 3, 3, 3]
    let indices = a.indices(of: 3)
    expectEqual(indices, [2..<3, 4..<6, 8..<9, 10..<13])
    
    let allTheThrees = a[indices]
    expectEqual(allTheThrees.count, 7)
    expectTrue(allTheThrees.allSatisfy { $0 == 3 })
    expectEqual(Array(allTheThrees), Array(repeating: 3, count: 7))
    
    let lowerIndices = letterString.indices(where: { $0.isLowercase })
    let lowerOnly = letterString[lowerIndices]
    expectEqualSequence(lowerOnly, lowercaseLetters)
    expectEqualSequence(lowerOnly.reversed(), lowercaseLetters.reversed())
    
    let upperOnly = letterString.removingSubranges(lowerIndices)
    expectEqualSequence(upperOnly, uppercaseLetters)
    expectEqualSequence(upperOnly.reversed(), uppercaseLetters.reversed())
  }

  RangeSetTests.test("removeSubranges") {
    var a = [1, 2, 3, 4, 3, 3, 4, 5, 3, 4, 3, 3, 3]
    let indices = a.indices(of: 3)
    a.removeSubranges(indices)
    expectEqual(a, [1, 2, 4, 4, 5, 4])
    
    var numbers = Array(1...20)
    numbers.removeSubranges(RangeSet([2..<5, 10..<15, 18..<20]))
    expectEqual(numbers, [1, 2, 6, 7, 8, 9, 10, 16, 17, 18])
    
    numbers = Array(1...20)
    numbers.removeSubranges([])
    expectEqual(numbers, Array(1...20))

    let sameNumbers = numbers.removingSubranges([])
    expectEqualSequence(numbers, sameNumbers)
    
    let noNumbers = numbers.removingSubranges(RangeSet(numbers.indices))
    expectEqualSequence(EmptyCollection(), noNumbers)
    
    var str = letterString
    let lowerIndices = str.indices(where: { $0.isLowercase })
    
    let upperOnly = str.removingSubranges(lowerIndices)
    expectEqualSequence(upperOnly, uppercaseLetters)
    
    str.removeSubranges(lowerIndices)
    expectEqualSequence(str, uppercaseLetters)
  }

  RangeSetTests.test("moveSubranges/rangeset") {
    // Move before
    var numbers = Array(1...20)
    let range1 = numbers.moveSubranges(RangeSet([10..<15, 18..<20]), to: 4)
    expectEqual(range1, 4..<11)
    expectEqual(numbers, [
      1, 2, 3, 4,
      11, 12, 13, 14, 15,
      19, 20,
      5, 6, 7, 8, 9, 10, 16, 17, 18])
    
    // Move to start
    numbers = Array(1...20)
    let range2 = numbers.moveSubranges(RangeSet([10..<15, 18..<20]), to: 0)
    expectEqual(range2, 0..<7)
    expectEqual(numbers, [
      11, 12, 13, 14, 15,
      19, 20,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 16, 17, 18])
    
    // Move to end
    numbers = Array(1...20)
    let range3 = numbers.moveSubranges(RangeSet([10..<15, 18..<20]), to: 20)
    expectEqual(range3, 13..<20)
    expectEqual(numbers, [
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 16, 17, 18,
      11, 12, 13, 14, 15,
      19, 20,
    ])
    
    // Move to middle of selected elements
    numbers = Array(1...20)
    let range4 = numbers.moveSubranges(RangeSet([10..<15, 18..<20]), to: 14)
    expectEqual(range4, 10..<17)
    expectEqual(numbers, [
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15,
      19, 20,
      16, 17, 18])
    
    // Move none
    numbers = Array(1...20)
    let range5 = numbers.moveSubranges(RangeSet(), to: 10)
    expectEqual(range5, 10..<10)
    expectEqual(numbers, Array(1...20))
  }

  RangeSetTests.test("moveSubranges/noCOW") {
    let numbers = Array(1...20)
    expectNoCopyOnWrite(numbers) { numbers in
      numbers.moveSubranges(RangeSet([10..<15, 18..<20]), to: 4)
    }
    expectNoCopyOnWrite(numbers) { numbers in
      numbers.removeSubranges(RangeSet([2..<5, 10..<15, 18..<20]))
    }
  }

  RangeSetTests.test("DiscontiguousSliceSequence") {
    let initial = 1...100
    
    // Build an array of ranges that include alternating groups of 5 elements
    // e.g. 1...5, 11...15, etc
    let rangeStarts = initial.indices.every(10)
    let rangeEnds = rangeStarts.compactMap {
      initial.index($0, offsetBy: 5, limitedBy: initial.endIndex)
    }
    let ranges = zip(rangeStarts, rangeEnds).map(Range.init)
    let set = RangeSet(ranges)
    let slice = initial[set]

    // Test various public, underscored members for the slice's
    // Sequence/Collection conformance
    expectTrue(slice.contains(2))
    expectFalse(slice.contains(7))
    expectEqualSequence(Array(slice), slice)
    expectEqual(slice.firstIndex(of: 2), slice.index(after: slice.startIndex))
    expectEqual(slice.firstIndex(of: 7), nil)
    expectEqual(slice.lastIndex(of: 2), slice.index(after: slice.startIndex))
    expectEqual(slice.lastIndex(of: 7), nil)
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
    
    for (chosenIdx, disIdx) in
      zip(chosenElements.indices, discontiguousSlice.indices)
    {
      expectEqualSequence(
        chosenElements[chosenIdx...],
        discontiguousSlice[disIdx...]
      )
      expectEqualSequence(
        chosenElements[..<chosenIdx],
        discontiguousSlice[..<disIdx]
      )

      for (chosenUpper, disUpper) in
        zip(
          chosenElements.indices[chosenIdx...],
          discontiguousSlice.indices[disIdx...]
        )
      {
        expectEqualSequence(
          Array(chosenElements[chosenIdx..<chosenUpper]),
          Array(discontiguousSlice[disIdx..<disUpper]))
      }
    }
  }

  RangeSetTests.test("DiscontiguousSliceIntermediateIndices") {
    do {
      let string = "Cafe\u{301} A"
      let indexOfAccent = string.unicodeScalars
        .index(string.startIndex, offsetBy: 4)

      var set = RangeSet<String.Index>()
      set.insert(indexOfAccent, within: string)
      expectEqualSequence(string[set], ["\u{301}"])
      expectEqualSequence(string[set].reversed(), ["\u{301}"])

      set.insert(string.startIndex, within: string)
      expectEqualSequence(string[set], ["C", "\u{301}"])
      expectEqualSequence(string[set].reversed(), ["\u{301}", "C"])

      set.insert(string.index(before: string.endIndex), within: string)
      expectEqualSequence(string[set], ["C", "\u{301}", "A"])
      expectEqualSequence(string[set].reversed(), ["A", "\u{301}", "C"])

      let indexOfE = string.index(string.startIndex, offsetBy: 3)
      let rangeOfE = indexOfE ..< indexOfAccent
      set.insert(contentsOf: rangeOfE)
      expectEqualSequence(string[set], ["C", "e\u{301}", "A"])
      expectEqualSequence(string[set].reversed(), ["A", "e\u{301}", "C"])
    }

    do {
      let string = Array(
        repeating: "\u{1F1E8}\u{1F1ED}\u{1F1FA}\u{1F1E6}",
        count: 4
      ).joined()

      let scalars = Array(string.unicodeScalars.indices)
      print(scalars.count)
      let r1 = string.startIndex ..< scalars[4]
      let r2 = scalars[5] ..< scalars[9]
      let r3 = scalars[10] ..< string.endIndex
      let set = RangeSet([r1, r2, r3])

      let expected: [Character] = [
        "\u{1F1E8}\u{1F1ED}", "\u{1F1FA}\u{1F1E6}",
        "\u{1F1ED}\u{1F1FA}", "\u{1F1E6}\u{1F1E8}",
        "\u{1F1FA}\u{1F1E6}", "\u{1F1E8}\u{1F1ED}", "\u{1F1FA}\u{1F1E6}"
      ]
      expectEqualSequence(string[set], expected)
      expectEqualSequence(string[set].reversed(), expected.reversed())
    }
  }

  RangeSetTests.test("InsertionReturningPreviousContainment") {
    do {
      var s = RangeSet<Int>()
      expectTrue(s.insert(20, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(-100, within: parent))
    }
    do {
      var s = source
      expectFalse(s.insert(20, within: parent))
    }
    do {
      var s = source
      expectFalse(s.insert(21, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(22, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(23, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(26, within: parent))
    }
    do {
      var s = source
      expectFalse(s.insert(27, within: parent))
    }
    do {
      var s = source
      expectFalse(s.insert(28, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(29, within: parent))
    }
    do {
      var s = source
      expectTrue(s.insert(100, within: parent))
    }
  }
}

runAllTests()
