//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct DropFirstTest {
  public var sequence: [Int]
  public let dropElements: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], dropElements: Int, expected: [Int],
      file: String = __FILE__, line: UInt = __LINE__) {
    self.sequence = sequence
    self.dropElements = dropElements
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "dropFirst() test data")
  }
}

public struct DropLastTest {
  public var sequence: [Int]
  public let dropElements: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], dropElements: Int, expected: [Int],
      file: String = __FILE__, line: UInt = __LINE__) {
    self.sequence = sequence
    self.dropElements = dropElements
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "dropLast() test data")
  }
}

public struct ElementsEqualTest {
  public let expected: Bool
  public let sequence: [Int]
  public let other: [Int]
  public let expectedLeftoverSequence: [Int]
  public let expectedLeftoverOther: [Int]
  public let loc: SourceLoc

  public init(
    _ expected: Bool, _ sequence: [Int], _ other: [Int],
    _ expectedLeftoverSequence: [Int],
    _ expectedLeftoverOther: [Int],
    file: String = __FILE__, line: UInt = __LINE__,
    comment: String = ""
  ) {
    self.expected = expected
    self.sequence = sequence
    self.other = other
    self.expectedLeftoverSequence = expectedLeftoverSequence
    self.expectedLeftoverOther = expectedLeftoverOther
    self.loc = SourceLoc(file, line, comment: "test data" + comment)
  }

  func flip() -> ElementsEqualTest {
    return ElementsEqualTest(
      expected, other, sequence,
      expectedLeftoverOther, expectedLeftoverSequence,
      file: loc.file, line: loc.line, comment: " (flipped)")
  }
}

public struct EnumerateTest {
  public let expected: [(Int, Int)]
  public let sequence: [Int]
  public let loc: SourceLoc

  public init(
    _ expected: [(Int, Int)], _ sequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__,
    comment: String = ""
  ) {
    self.expected = expected
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data" + comment)
  }
}

public struct FilterTest {
  public let expected: [Int]
  public let sequence: [Int]
  public let includeElement: (Int) -> Bool
  public let loc: SourceLoc

  public init(
    _ expected: [Int],
    _ sequence: [Int],
    _ includeElement: (Int) -> Bool,
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.includeElement = includeElement
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct FindTest {
  public let expected: Int?
  public let element: MinimalEquatableValue
  public let sequence: [MinimalEquatableValue]
  public let expectedLeftoverSequence: [MinimalEquatableValue]
  public let loc: SourceLoc

  public init(
    expected: Int?, element: Int, sequence: [Int],
    expectedLeftoverSequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.element = MinimalEquatableValue(element)
    self.sequence = sequence.map(MinimalEquatableValue.init)
    self.expectedLeftoverSequence = expectedLeftoverSequence.map(
      MinimalEquatableValue.init)
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct FlatMapTest {
  public let expected: [Int32]
  public let sequence: [Int]
  public let transform: (Int) -> [Int32]
  public let loc: SourceLoc

  public init(
    expected: [Int32],
    sequence: [Int],
    transform: (Int) -> [Int32],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.transform = transform
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct FlatMapToOptionalTest {
  public let expected: [Int32]
  public let sequence: [Int]
  public let transform: (Int) -> Int32?
  public let loc: SourceLoc

  public init(
    _ expected: [Int32],
    _ sequence: [Int],
    _ transform: (Int) -> Int32?,
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.transform = transform
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

/// This test performs a side effect on each element of `sequence`. The expected
/// side effect for the purposes of testing is to append each element to an
/// external array, which can be compared to `sequence`.
internal struct ForEachTest {
  let sequence: [Int]
  let loc: SourceLoc

  init(
    _ sequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct LexicographicalCompareTest {
  public let expected: ExpectedComparisonResult
  public let sequence: [Int]
  public let other: [Int]
  public let expectedLeftoverSequence: [Int]
  public let expectedLeftoverOther: [Int]
  public let loc: SourceLoc

  public init(
    _ expected: ExpectedComparisonResult, _ sequence: [Int], _ other: [Int],
    _ expectedLeftoverSequence: [Int],
    _ expectedLeftoverOther: [Int],
    file: String = __FILE__, line: UInt = __LINE__,
    comment: String = ""
  ) {
    self.expected = expected
    self.sequence = sequence
    self.other = other
    self.expectedLeftoverSequence = expectedLeftoverSequence
    self.expectedLeftoverOther = expectedLeftoverOther
    self.loc = SourceLoc(file, line, comment: "test data" + comment)
  }

  func flip() -> LexicographicalCompareTest {
    return LexicographicalCompareTest(
      expected.flip(), other, sequence,
      expectedLeftoverOther, expectedLeftoverSequence,
      file: loc.file, line: loc.line, comment: " (flipped)")
  }
}

public struct MapTest {
  public let expected: [Int32]
  public let sequence: [Int]
  public let transform: (Int) -> Int32
  public let loc: SourceLoc

  public init(
    _ expected: [Int32],
    _ sequence: [Int],
    _ transform: (Int) -> Int32,
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.transform = transform
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct MinMaxElementTest {
  public let expectedMinValue: Int?
  public let expectedMinIndex: Int?
  public let expectedMaxValue: Int?
  public let expectedMaxIndex: Int?
  public let sequence: [Int]
  public let loc: SourceLoc

  public init(
    minValue expectedMinValue: Int?,
    index expectedMinIndex: Int?,
    maxValue expectedMaxValue: Int?,
    index expectedMaxIndex: Int?,
    _ sequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__,
    comment: String = ""
  ) {
    self.expectedMinValue = expectedMinValue
    self.expectedMinIndex = expectedMinIndex
    self.expectedMaxValue = expectedMaxValue
    self.expectedMaxIndex = expectedMaxIndex
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data" + comment)
  }
}

public struct PrefixTest {
  public var sequence: [Int]
  public let maxLength: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], maxLength: Int, expected: [Int],
      file: String = __FILE__, line: UInt = __LINE__) {
    self.sequence = sequence
    self.maxLength = maxLength
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "prefix() test data")
  }
}

public struct ReduceTest {
  public let sequence: [Int]
  public let loc: SourceLoc

  public init(
    _ sequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct ReverseTest {
  public let expected: [Int]
  public let sequence: [Int]
  public let loc: SourceLoc

  public init(
    _ expected: [Int], _ sequence: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct SuffixTest {
  public var sequence: [Int]
  public let maxLength: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], maxLength: Int, expected: [Int],
      file: String = __FILE__, line: UInt = __LINE__) {
    self.sequence = sequence
    self.maxLength = maxLength
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "suffix() test data")
  }
}

public struct SplitTest {
  public var sequence: [Int]
  public let maxSplit: Int
  public let separator: Int
  public let allowEmptySlices: Bool
  public let expected: [[Int]]
  public let loc: SourceLoc

  public init(sequence: [Int], maxSplit: Int, separator: Int, expected: [[Int]],
       allowEmptySlices: Bool, file: String = __FILE__, line: UInt = __LINE__) {
    self.sequence = sequence
    self.maxSplit = maxSplit
    self.separator = separator
    self.allowEmptySlices = allowEmptySlices
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "suffix() test data")
  }
}

public struct StartsWithTest {
  public let expected: Bool
  public let sequence: [Int]
  public let prefix: [Int]
  public let expectedLeftoverSequence: [Int]
  public let expectedLeftoverPrefix: [Int]
  public let loc: SourceLoc

  public init(
    _ expected: Bool, _ sequence: [Int], _ prefix: [Int],
    _ expectedLeftoverSequence: [Int],
    _ expectedLeftoverPrefix: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.prefix = prefix
    self.expectedLeftoverSequence = expectedLeftoverSequence
    self.expectedLeftoverPrefix = expectedLeftoverPrefix
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}
public struct ZipTest {
  public let expected: [(Int, Int32)]
  public let sequence: [Int]
  public let other: [Int32]
  public let expectedLeftoverSequence: [Int]
  public let expectedLeftoverOther: [Int32]
  public let loc: SourceLoc

  public init(
    _ expected: [(Int, Int32)],
    sequences sequence: [Int],
    _ other: [Int32],
    leftovers expectedLeftoverSequence: [Int],
    _ expectedLeftoverOther: [Int32],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected
    self.sequence = sequence
    self.other = other
    self.expectedLeftoverSequence = expectedLeftoverSequence
    self.expectedLeftoverOther = expectedLeftoverOther
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}


public let elementsEqualTests: [ElementsEqualTest] = [
  ElementsEqualTest(true, [], [], [], []),

  ElementsEqualTest(false, [ 1 ], [], [], []),
  ElementsEqualTest(false, [], [ 1 ], [], []),

  ElementsEqualTest(false, [ 1, 2 ], [], [ 2 ], []),
  ElementsEqualTest(false, [], [ 1, 2 ], [], [ 2 ]),

  ElementsEqualTest(false, [ 1, 2, 3, 4 ], [ 1, 2 ], [ 4 ], []),
  ElementsEqualTest(false, [ 1, 2 ], [ 1, 2, 3, 4 ], [], [ 4 ]),
].flatMap { [ $0, $0.flip() ] }

public let enumerateTests = [
  EnumerateTest([], []),
  EnumerateTest([ (0, 10) ], [ 10 ]),
  EnumerateTest([ (0, 10), (1, 20) ], [ 10, 20 ]),
  EnumerateTest([ (0, 10), (1, 20), (2, 30) ], [ 10, 20, 30 ]),
]

public let filterTests = [
  FilterTest(
    [], [],
    { (x: Int) -> Bool in expectUnreachable(); return true }),

  FilterTest([], [ 0, 30, 10, 90 ], { (x: Int) -> Bool in false }),
  FilterTest(
    [ 0, 30, 10, 90 ], [ 0, 30, 10, 90 ], { (x: Int) -> Bool in true }
  ),
  FilterTest(
    [ 0, 30, 90 ], [ 0, 30, 10, 90 ], { (x: Int) -> Bool in x % 3 == 0 }
  ),
]

public let findTests = [
  FindTest(
    expected: nil,
    element: 42,
    sequence: [],
    expectedLeftoverSequence: []),

  FindTest(
    expected: nil,
    element: 42,
    sequence: [ 1010 ],
    expectedLeftoverSequence: []),
  FindTest(
    expected: 0,
    element: 1010,
    sequence: [ 1010 ],
    expectedLeftoverSequence: []),

  FindTest(
    expected: nil,
    element: 42,
    sequence: [ 1010, 1010 ],
    expectedLeftoverSequence: []),
  FindTest(
    expected: 0,
    element: 1010,
    sequence: [ 1010, 1010 ],
    expectedLeftoverSequence: [ 1010 ]),

  FindTest(
    expected: nil,
    element: 42,
    sequence: [ 1010, 2020, 3030, 4040 ],
    expectedLeftoverSequence: []),
  FindTest(
    expected: 0,
    element: 1010,
    sequence: [ 1010, 2020, 3030, 4040 ],
    expectedLeftoverSequence: [ 2020, 3030, 4040 ]),
  FindTest(
    expected: 1,
    element: 2020,
    sequence: [ 1010, 2020, 3030, 4040 ],
    expectedLeftoverSequence: [ 3030, 4040 ]),
  FindTest(
    expected: 2,
    element: 3030,
    sequence: [ 1010, 2020, 3030, 4040 ],
    expectedLeftoverSequence: [ 4040 ]),
  FindTest(
    expected: 3,
    element: 4040,
    sequence: [ 1010, 2020, 3030, 4040 ],
    expectedLeftoverSequence: []),

  FindTest(
    expected: 1,
    element: 2020,
    sequence: [ 1010, 2020, 3030, 2020, 4040 ],
    expectedLeftoverSequence: [ 3030, 2020, 4040 ]),
]

/// For a number of form `NNN_MMM`, returns an array of `NNN` numbers that all
/// have `MMM` as their last three digits.
func flatMapTransformation(x: Int) -> [Int32] {
  let repetitions = x / 1000
  let identity = x % 1000
  let range = (1..<(repetitions+1))
  return range.map { Int32($0 * 1000 + identity) }
}

public let flatMapTests = [
  FlatMapTest(
    expected: [],
    sequence: [],
    transform: { (x: Int) -> [Int32] in
      expectUnreachable()
      return [ 0xffff ]
    }),

  FlatMapTest(
    expected: [],
    sequence: [ 1 ],
    transform: { (x: Int) -> [Int32] in [] }),
  FlatMapTest(
    expected: [],
    sequence: [ 1, 2 ],
    transform: { (x: Int) -> [Int32] in [] }),
  FlatMapTest(
    expected: [],
    sequence: [ 1, 2, 3 ],
    transform: { (x: Int) -> [Int32] in [] }),

  FlatMapTest(
    expected: [ 101 ],
    sequence: [ 1 ],
    transform: { (x: Int) -> [Int32] in [ x + 100 ] }),
  FlatMapTest(
    expected: [ 101, 102 ],
    sequence: [ 1, 2 ],
    transform: { (x: Int) -> [Int32] in [ x + 100 ] }),
  FlatMapTest(
    expected: [ 101, 102, 103 ],
    sequence: [ 1, 2, 3 ],
    transform: { (x: Int) -> [Int32] in [ x + 100 ] }),

  FlatMapTest(
    expected: [ 101, 201 ],
    sequence: [ 1 ],
    transform: { (x: Int) -> [Int32] in [ x + 100, x + 200 ] }),
  FlatMapTest(
    expected: [ 101, 201, 102, 202 ],
    sequence: [ 1, 2 ],
    transform: { (x: Int) -> [Int32] in [ x + 100, x + 200 ] }),
  FlatMapTest(
    expected: [ 101, 201, 102, 202, 103, 203 ],
    sequence: [ 1, 2, 3 ],
    transform: { (x: Int) -> [Int32] in [ x + 100, x + 200 ] }),

  FlatMapTest(
    expected: [ 1_071, 1_075 ],
    sequence: [ 1_071, 72, 73, 74, 1_075 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_072, 1_073, 2_073 ],
    sequence: [ 1, 1_072, 2_073 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_071, 2_071, 1_073, 2_073, 3_073, 1_074 ],
    sequence: [ 2_071, 2, 3_073, 1_074 ],
    transform: flatMapTransformation),

  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1, 2, 1_073, 4, 5, 2_076, 7, 8, 3_079, 10, 11 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1_073, 4, 5, 2_076, 7, 8, 3_079, 10, 11 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1, 2, 1_073, 4, 5, 2_076, 7, 8, 3_079 ],
    transform: flatMapTransformation),

  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1, 1_073, 4, 2_076, 7, 3_079, 10 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1_073, 4, 2_076, 7, 3_079, 10 ],
    transform: flatMapTransformation),
  FlatMapTest(
    expected: [ 1_073, 1_076, 2_076, 1_079, 2_079, 3_079 ],
    sequence: [ 1, 1_073, 4, 2_076, 7, 3_079 ],
    transform: flatMapTransformation),
]

public let flatMapToOptionalTests = [
  FlatMapToOptionalTest(
    [], [],
    { (x: Int) -> Int32? in expectUnreachable(); return 0xffff }),

  FlatMapToOptionalTest([], [ 1 ], { (x: Int) -> Int32? in nil }),
  FlatMapToOptionalTest([], [ 1, 2 ], { (x: Int) -> Int32? in nil }),
  FlatMapToOptionalTest([], [ 1, 2, 3 ], { (x: Int) -> Int32? in nil }),

  FlatMapToOptionalTest(
    [ 1 ], [ 1 ],
    { (x: Int) -> Int32? in x > 10 ? nil : Int32(x) }),
  FlatMapToOptionalTest(
    [ 2 ], [ 11, 2, 13, 14 ],
    { (x: Int) -> Int32? in x > 10 ? nil : Int32(x) }),
  FlatMapToOptionalTest(
    [ 1, 4 ], [ 1, 12, 13, 4 ],
    { (x: Int) -> Int32? in x > 10 ? nil : Int32(x) }),
  FlatMapToOptionalTest(
    [ 1, 2, 3 ], [ 1, 2, 3 ],
    { (x: Int) -> Int32? in x > 10 ? nil : Int32(x) }),
]


public let dropFirstTests = [
  DropFirstTest(
    sequence: [],
    dropElements: 0,
    expected: []
  ),
  DropFirstTest(
    sequence: [1010, 2020, 3030],
    dropElements: 1,
    expected: [2020, 3030]
  ),
  DropFirstTest(
    sequence: [1010, 2020, 3030],
    dropElements: 2,
    expected: [3030]
  ),
  DropFirstTest(
    sequence: [1010, 2020, 3030],
    dropElements: 3,
    expected: []
  ),
  DropFirstTest(
    sequence: [1010, 2020, 3030],
    dropElements: 777,
    expected: []
  ),
  DropFirstTest(
    sequence: [1010, 2020, 3030],
    dropElements: 0,
    expected: [1010, 2020, 3030]
  ),
]

public let dropLastTests = [
  DropLastTest(
    sequence: [],
    dropElements: 0,
    expected: []
  ),
  DropLastTest(
    sequence: [1010, 2020, 3030],
    dropElements: 1,
    expected: [1010, 2020]
  ),
  DropLastTest(
    sequence: [1010, 2020, 3030],
    dropElements: 2,
    expected: [1010]
  ),
  DropLastTest(
    sequence: [1010, 2020, 3030],
    dropElements: 3,
    expected: []
  ),
  DropLastTest(
    sequence: [1010, 2020, 3030],
    dropElements: 777,
    expected: []
  ),
  DropLastTest(
    sequence: [1010, 2020, 3030],
    dropElements: 0,
    expected: [1010, 2020, 3030]
  ),
]

public let lexicographicalCompareTests = [
  LexicographicalCompareTest(.EQ, [], [], [], []),
  LexicographicalCompareTest(.EQ, [ 1 ], [ 1 ], [], []),

  LexicographicalCompareTest(.GT, [ 1 ], [], [], []),

  LexicographicalCompareTest(.GT, [ 1 ], [ 0 ], [], []),
  LexicographicalCompareTest(.EQ, [ 1 ], [ 1 ], [], []),
  LexicographicalCompareTest(.LT, [ 1 ], [ 2 ], [], []),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [], [ 2 ], []),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 0 ], [ 2 ], []),
  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 1 ], [], []),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 2 ], [ 2 ], []),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 0, 0 ], [ 2 ], [ 0 ]),
  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 1, 0 ], [], []),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 2, 0 ], [ 2 ], [ 0 ]),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 0, 1 ], [ 2 ], [ 1 ]),
  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 1, 1 ], [], []),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 2, 1 ], [ 2 ], [ 1 ]),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 0, 2 ], [ 2 ], [ 2 ]),
  LexicographicalCompareTest(.EQ, [ 1, 2 ], [ 1, 2 ], [], []),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 2, 2 ], [ 2 ], [ 2 ]),

  LexicographicalCompareTest(.GT, [ 1, 2 ], [ 0, 3 ], [ 2 ], [ 3 ]),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 1, 3 ], [], []),
  LexicographicalCompareTest(.LT, [ 1, 2 ], [ 2, 3 ], [ 2 ], [ 3 ]),
].flatMap { [ $0, $0.flip() ] }

public let mapTests = [
  MapTest(
    [], [],
    { (x: Int) -> Int32 in expectUnreachable(); return 0xffff }),

  MapTest([ 101 ], [ 1 ], { (x: Int) -> Int32 in x + 100 }),
  MapTest([ 101, 102 ], [ 1, 2 ], { (x: Int) -> Int32 in x + 100 }),
  MapTest([ 101, 102, 103 ], [ 1, 2, 3 ], { (x: Int) -> Int32 in x + 100 }),
  MapTest(Array(101..<200), Array(1..<100), { (x: Int) -> Int32 in x + 100 }),
]

public let minMaxElementTests = [
  MinMaxElementTest(
    minValue: nil, index: nil,
    maxValue: nil, index: nil,
    []),
  MinMaxElementTest(
    minValue: 42, index: 0,
    maxValue: 42, index: 0,
    [ 42 ]),
  MinMaxElementTest(
    minValue: -1, index: 1,
    maxValue: 30, index: 2,
    [ 10, -1, 30, -1, 30 ]),
  MinMaxElementTest(
    minValue: -2, index: 5,
    maxValue: 31, index: 6,
    [ 10, -1, 30, -1, 30, -2, 31 ]),
]

public let reduceTests = [
  ReduceTest([]),
  ReduceTest([ 1 ]),
  ReduceTest([ 1, 2 ]),
  ReduceTest([ 1, 2, 3 ]),
  ReduceTest([ 1, 2, 3, 4, 5, 6, 7 ]),
]

public let reverseTests: [ReverseTest] = [
  ReverseTest([], []),
  ReverseTest([ 1 ], [ 1 ]),
  ReverseTest([ 2, 1 ], [ 1, 2 ]),
  ReverseTest([ 3, 2, 1 ], [ 1, 2, 3 ]),
  ReverseTest([ 4, 3, 2, 1 ], [ 1, 2, 3, 4]),
  ReverseTest(
    [ 7, 6, 5, 4, 3, 2, 1 ],
    [ 1, 2, 3, 4, 5, 6, 7 ]),
]

public let splitTests: [SplitTest] = [
  //
  // Empty sequence.
  //

  // Empty sequence, maxSplit == 0.
  SplitTest(
    sequence: [],
    maxSplit: 0,
    separator: 99,
    expected: [[]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [],
    maxSplit: 0,
    separator: 99,
    expected: [],
    allowEmptySlices: false
  ),

  // Empty sequence, maxSplit == 1.
  SplitTest(
    sequence: [],
    maxSplit: 1,
    separator: 99,
    expected: [[]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [],
    maxSplit: 1,
    separator: 99,
    expected: [],
    allowEmptySlices: false
  ),

  // Empty sequence, maxSplit == Int.max.
  SplitTest(
    sequence: [],
    maxSplit: Int.max,
    separator: 99,
    expected: [[]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [],
    maxSplit: Int.max,
    separator: 99,
    expected: [],
    allowEmptySlices: false
  ),

  //
  // 1-element sequence.
  //

  // 1-element sequence, maxSplit == 0.
  SplitTest(
    sequence: [1],
    maxSplit: 0,
    separator: 1,
    expected: [[1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 0,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 0,
    separator: 1,
    expected: [[1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 0,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: false
  ),

  // 1-element sequence, maxSplit == 1.
  SplitTest(
    sequence: [1],
    maxSplit: 1,
    separator: 1,
    expected: [[], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 1,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 1,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1],
    maxSplit: 1,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: false
  ),

  // 1-element sequence, maxSplit == Int.max.
  SplitTest(
    sequence: [1],
    maxSplit: Int.max,
    separator: 1,
    expected: [[], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: Int.max,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1],
    maxSplit: Int.max,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1],
    maxSplit: Int.max,
    separator: 99,
    expected: [[1]],
    allowEmptySlices: false
  ),

  //
  // 2-element sequence [1, 2].
  //

  // 2-element sequence [1, 2], maxSplit == 0.
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 1,
    expected: [[1, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 2,
    expected: [[1, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 1,
    expected: [[1, 2]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 2,
    expected: [[1, 2]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 0,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: false
  ),

  // 2-element sequence [1, 2], maxSplit == 1.
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 1,
    expected: [[], [2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 2,
    expected: [[1], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 1,
    expected: [[2]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 2,
    expected: [[1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: 1,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: false
  ),

  // 2-element sequence [1, 2], maxSplit == Int.max.
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 1,
    expected: [[], [2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 1,
    expected: [[2]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2],
    maxSplit: Int.max,
    separator: 99,
    expected: [[1, 2]],
    allowEmptySlices: false
  ),

  //
  // 2-element sequence [1, 1].
  //

  // 2-element sequence [1, 1], maxSplit == 0.
  SplitTest(
    sequence: [1, 1],
    maxSplit: 0,
    separator: 1,
    expected: [[1, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 0,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 0,
    separator: 1,
    expected: [[1, 1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 0,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: false
  ),

  // 2-element sequence [1, 1], maxSplit == 1.
  SplitTest(
    sequence: [1, 1],
    maxSplit: 1,
    separator: 1,
    expected: [[], [1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 1,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 1,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 1,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: false
  ),

  // 2-element sequence [1, 1], maxSplit == 2.
  SplitTest(
    sequence: [1, 1],
    maxSplit: 2,
    separator: 1,
    expected: [[], [], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 2,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 2,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1],
    maxSplit: 2,
    separator: 99,
    expected: [[1, 1]],
    allowEmptySlices: false
  ),

  //
  // 3-element sequence [1, 1, 1].
  //

  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 1,
    separator: 1,
    expected: [[], [1, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 2,
    separator: 1,
    expected: [[], [], [1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 3,
    separator: 1,
    expected: [[], [], [], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: Int.max,
    separator: 1,
    expected: [[], [], [], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 1,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 2,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: 3,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 1, 1],
    maxSplit: Int.max,
    separator: 1,
    expected: [],
    allowEmptySlices: false
  ),

  //
  // Other tests.
  //

  SplitTest(
    sequence: [1, 2, 2, 2, 1],
    maxSplit: 1,
    separator: 2,
    expected: [[1], [2, 2, 1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2, 1],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [], [], [1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2, 1],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [], [], [1], [], [], [1], [], [], [1]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [1], [1], [1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2],
    maxSplit: Int.max,
    separator: 2,
    expected:
      [[], [], [], [1], [], [], [1], [], [], [1], [], [], [1], [], [], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [1], [1], [1]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2, 3, 4, 5, 6, 7],
    maxSplit: 4,
    separator: 4,
    expected: [[1, 2, 3], [5, 6, 7]],
    allowEmptySlices: false
  ),
  SplitTest(
    sequence: [1, 2, 3, 3, 4, 5, 6, 7],
    maxSplit: 3,
    separator: 3,
    expected: [[1, 2], [], [4, 5, 6, 7]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2, 2, 2, 2, 2, 2],
    maxSplit: 3,
    separator: 2,
    expected: [[1], [], [], [2, 2, 2, 2, 2]],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2],
    maxSplit: Int.max,
    separator: 2,
    expected: [[1], [], [], []],
    allowEmptySlices: true
  ),
  SplitTest(
    sequence: [1, 2, 2, 2],
    maxSplit: Int.max,
    separator: 3,
    expected: [[1, 2, 2, 2]],
    allowEmptySlices: true
  ),
]

public let prefixTests = [
  PrefixTest(
    sequence: [],
    maxLength: 0,
    expected: []
  ),
  PrefixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 1,
    expected: [1010]
  ),
  PrefixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 2,
    expected: [1010, 2020]
  ),
  PrefixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 3,
    expected: [1010, 2020, 3030]
  ),
  PrefixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 777,
    expected: [1010, 2020, 3030]
  ),
  PrefixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 0,
    expected: []
  ),
  PrefixTest(
    sequence: [2, 3, 5, 7, 11],
    maxLength: 6,
    expected: [2, 3, 5, 7, 11]
  ),
  PrefixTest(
    sequence: [2, 3, 5, 7, 11],
    maxLength: 5,
    expected: [2, 3, 5, 7, 11]
  ),
]

public let startsWithTests = [
  // Corner cases.
  StartsWithTest(true, [], [], [], []),

  StartsWithTest(false, [], [ 1 ], [], []),
  StartsWithTest(true, [ 1 ], [], [], []),

  // Equal sequences.
  StartsWithTest(true, [ 1 ], [ 1 ], [], []),
  StartsWithTest(true, [ 1, 2 ], [ 1, 2 ], [], []),

  // Proper prefix.
  StartsWithTest(true, [ 0, 1, 2 ], [ 0, 1 ], [], []),
  StartsWithTest(false, [ 0, 1 ], [ 0, 1, 2 ], [], []),

  StartsWithTest(true, [ 1, 2, 3, 4 ], [ 1, 2 ], [ 4 ], []),
  StartsWithTest(false, [ 1, 2 ], [ 1, 2, 3, 4 ], [], [ 4 ]),

  // Not a prefix.
  StartsWithTest(false, [ 1, 2, 3, 4 ], [ 1, 2, 10 ], [ 4 ], []),
  StartsWithTest(false, [ 1, 2, 10 ], [ 1, 2, 3, 4 ], [], [ 4 ]),

  StartsWithTest(false, [ 1, 2, 3, 4, 10 ], [ 1, 2, 10 ], [ 4, 10 ], []),
  StartsWithTest(false, [ 1, 2, 10 ], [ 1, 2, 3, 4, 10 ], [], [ 4, 10 ]),
]


public let suffixTests = [
  SuffixTest(
    sequence: [],
    maxLength: 0,
    expected: []
  ),
  SuffixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 1,
    expected: [3030]
  ),
  SuffixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 2,
    expected: [2020, 3030]
  ),
  SuffixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 3,
    expected: [1010, 2020, 3030]
  ),
  SuffixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 777,
    expected: [1010, 2020, 3030]
  ),
  SuffixTest(
    sequence: [1010, 2020, 3030],
    maxLength: 0,
    expected: []
  ),
]

public let zipTests = [
  ZipTest([], sequences: [], [], leftovers: [], []),
  ZipTest([], sequences: [], [ 1 ], leftovers: [], [ 1 ]),
  ZipTest([], sequences: [], [ 1, 2 ], leftovers: [], [ 1, 2 ]),
  ZipTest([], sequences: [], [ 1, 2, 3 ], leftovers: [], [ 1, 2, 3 ]),

  ZipTest([], sequences: [ 10 ], [], leftovers: [], []),
  ZipTest([ (10, 1) ], sequences: [ 10 ], [ 1 ], leftovers: [], []),
  ZipTest([ (10, 1) ], sequences: [ 10 ], [ 1, 2 ], leftovers: [], [ 2 ]),
  ZipTest([ (10, 1) ], sequences: [ 10 ], [ 1, 2, 3 ], leftovers: [], [ 2, 3 ]),

  ZipTest(
    [],
    sequences: [ 10, 20 ], [],
    leftovers: [ 20 ], []),
  ZipTest(
    [ (10, 1) ],
    sequences: [ 10, 20 ], [ 1 ],
    leftovers: [], []),
  ZipTest(
    [ (10, 1), (20, 2) ],
    sequences: [ 10, 20 ], [ 1, 2 ],
    leftovers: [], []),
  ZipTest(
    [ (10, 1), (20, 2) ],
    sequences: [ 10, 20 ], [ 1, 2, 3 ],
    leftovers: [], [ 3 ]),

  ZipTest(
    [],
    sequences: [ 10, 20, 30 ], [],
    leftovers: [ 20, 30 ], []),
  ZipTest(
    [ (10, 1) ],
    sequences: [ 10, 20, 30 ], [ 1 ],
    leftovers: [ 30 ], []),
  ZipTest(
    [ (10, 1), (20, 2) ],
    sequences: [ 10, 20, 30 ], [ 1, 2 ],
    leftovers: [], []),
  ZipTest(
    [ (10, 1), (20, 2), (30, 3) ],
    sequences: [ 10, 20, 30 ], [ 1, 2, 3 ],
    leftovers: [], []),
]

public func callGenericUnderestimatedCount<S : SequenceType>(s: S) -> Int {
  return s.underestimateCount()
}

extension TestSuite {
  public func addSequenceTests<
    Sequence : SequenceType,
    SequenceWithEquatableElement : SequenceType
    where
    SequenceWithEquatableElement.Generator.Element : Equatable,
    Sequence.SubSequence : SequenceType,
    Sequence.SubSequence.Generator.Element == Sequence.Generator.Element,
    Sequence.SubSequence.SubSequence == Sequence.SubSequence
  >(
    testNamePrefix: String = "",
    makeSequence: ([Sequence.Generator.Element]) -> Sequence,
    wrapValue: (OpaqueValue<Int>) -> Sequence.Generator.Element,
    extractValue: (Sequence.Generator.Element) -> OpaqueValue<Int>,

    makeSequenceOfEquatable: ([SequenceWithEquatableElement.Generator.Element]) -> SequenceWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> SequenceWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((SequenceWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all
  ) {

    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(__FUNCTION__) {
      return
    }
    checksAdded.value.insert(__FUNCTION__)

    func makeWrappedSequence(elements: [OpaqueValue<Int>]) -> Sequence {
      return makeSequence(elements.map(wrapValue))
    }

    func makeWrappedSequenceWithEquatableElement(
      elements: [MinimalEquatableValue]
    ) -> SequenceWithEquatableElement {
      return makeSequenceOfEquatable(elements.map(wrapValueIntoEquatable))
    }

    testNamePrefix += String(Sequence.Type)

    let isMultiPass = makeSequence([])
      ._preprocessingPass { _ in true } ?? false
    let isEquatableMultiPass = makeSequenceOfEquatable([])
      ._preprocessingPass { _ in true } ?? false
    expectEqual(
      isMultiPass, isEquatableMultiPass,
      "Two sequence types are of different kinds?")

//===----------------------------------------------------------------------===//
// contains()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).contains()/WhereElementIsEquatable/semantics") {
  for test in findTests {
    let s = makeWrappedSequenceWithEquatableElement(test.sequence)

    expectEqual(
      test.expected != nil,
      s.contains(wrapValueIntoEquatable(test.element)),
      stackTrace: SourceLocStack().with(test.loc))

    if !isMultiPass {
      expectEqualSequence(
        test.expectedLeftoverSequence, s.map(extractValueFromEquatable),
        stackTrace: SourceLocStack().with(test.loc))
    }
  }
}

//===----------------------------------------------------------------------===//
// dropFirst()
//===----------------------------------------------------------------------===//
self.test("\(testNamePrefix).dropFirst/semantics") {
  for test in dropFirstTests {
    let s = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    let result = s.dropFirst(test.dropElements)
    expectEqualSequence(
      test.expected, result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).dropFirst/semantics/equivalence") {
  // Calling dropFirst(1) twice on a sequence should be the same as
  // calling dropFirst(2) once on an equivalent sequence.
  if true {
    let s1 = makeWrappedSequence([1010, 2020, 3030, 4040].map(OpaqueValue.init))
    let s2 = makeWrappedSequence([1010, 2020, 3030, 4040].map(OpaqueValue.init))

    let result0 = s1.dropFirst(1)
    let result1 = result0.dropFirst(1)
    let result2 = s2.dropFirst(2)

    expectEqualSequence(
      result1.map { extractValue($0).value },
      result2.map { extractValue($0).value }
    )
  }
}

self.test("\(testNamePrefix).dropFirst/semantics/dropFirst()==dropFirst(1)") {
  let s1 = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))
  let s2 = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))

  let result1 = s1.dropFirst()
  let result2 = s2.dropFirst(1)

  expectEqualSequence(
    result1.map { extractValue($0).value },
    result2.map { extractValue($0).value }
  )
}

self.test("\(testNamePrefix).dropFirst/semantics/negative") {
  let s = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  _ = s.dropFirst(-1)
}

//===----------------------------------------------------------------------===//
// dropLast()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).dropLast/semantics") {
  for test in dropLastTests {
    let s = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    let result = s.dropLast(test.dropElements)
    expectEqualSequence(test.expected, result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).dropLast/semantics/equivalence") {
  // Calling `dropLast(2)` twice on a sequence is equivalent to calling
  // `dropLast(4)` once.
  if true {
    let s1 = makeWrappedSequence(
      [1010, 2020, 3030, 4040, 5050].map(OpaqueValue.init))
    let s2 = makeWrappedSequence(
      [1010, 2020, 3030, 4040, 5050].map(OpaqueValue.init))

    let droppedOnce = s1.dropLast(4)
    let droppedTwice = s2.dropLast(2).dropLast(2)

    expectEqualSequence(droppedOnce, droppedTwice) {
      extractValue($0).value == extractValue($1).value
    }
  }
}

self.test("\(testNamePrefix).dropLast/semantics/negative") {
  let s = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  _ = s.dropLast(-1)
}

//===----------------------------------------------------------------------===//
// prefix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).prefix/semantics") {
  for test in prefixTests {
    let s = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    let result = s.prefix(test.maxLength)
    expectEqualSequence(test.expected, result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).prefix/semantics/equivalence") {
  // Calling `prefix(3)` on a sequence twice in a row should be the same
  // as calling it once.
  let expected = [
    1010, 2020, 3030, 4040, 5050,
    6060, 7070, 8080, 9090, 10010
  ].map(OpaqueValue.init)

  let s1 = makeWrappedSequence(expected)
  let s2 = makeWrappedSequence(expected)

  let prefixedOnce = s1.prefix(3)
  let temp = s2.prefix(3)
  let prefixedTwice = temp.prefix(3)

  expectEqualSequence(prefixedOnce, prefixedTwice) {
    extractValue($0).value == extractValue($1).value
  }
}

self.test("\(testNamePrefix).prefix/semantics/negative") {
  let s = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  _ = s.prefix(-1)
}

//===----------------------------------------------------------------------===//
// suffix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).suffix/semantics") {
  for test in suffixTests {
    let s = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    let result = s.suffix(test.maxLength)
    expectEqualSequence(test.expected, result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).suffix/semantics/equivalence") {
  // Calling `suffix(3)` on a sequence twice in a row should be the same
  // as calling it once.
  let expected = [
    1010, 2020, 3030, 4040, 5050,
    6060, 7070, 8080, 9090, 10010
  ].map(OpaqueValue.init)

  let s1 = makeWrappedSequence(expected)
  let s2 = makeWrappedSequence(expected)

  let prefixedOnce = s1.suffix(3)
  let temp = s2.suffix(3)
  let prefixedTwice = temp.prefix(3)

  expectEqualSequence(prefixedOnce, prefixedTwice) {
    extractValue($0).value == extractValue($1).value
  }
}

self.test("\(testNamePrefix).suffix/semantics/negative") {
  let s = makeWrappedSequence([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  _ = s.suffix(-1)
}

//===----------------------------------------------------------------------===//
// split()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).split/closure/semantics") {
  for test in splitTests {
    let closureLifetimeTracker = LifetimeTracked(0)
    expectEqual(1, LifetimeTracked.instances)
    let s: Sequence = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    let result = s.split(test.maxSplit,
      allowEmptySlices: test.allowEmptySlices) {
      _blackHole(closureLifetimeTracker)
      return extractValue($0).value == test.separator
    }
    expectEqualSequence(test.expected, result.map {
      $0.map {
        extractValue($0).value
      }
    },
    stackTrace: SourceLocStack().with(test.loc)) { $0 == $1 }
  }
}

self.test("\(testNamePrefix).split/separator/semantics") {
  for test in splitTests {
    let s = makeWrappedSequenceWithEquatableElement(
      test.sequence.map(MinimalEquatableValue.init)
    )
    let separator = wrapValueIntoEquatable(MinimalEquatableValue(test.separator))
    let result = s.split(
      separator,
      maxSplit: test.maxSplit,
      allowEmptySlices: test.allowEmptySlices)
    expectEqualSequence(
      test.expected,
      result.map {
        $0.map {
          extractValueFromEquatable($0).value
        }
      },
      stackTrace: SourceLocStack().with(test.loc)) { $0 == $1 }
  }
}

self.test("\(testNamePrefix).split/semantics/closure/negativeMaxSplit") {
  let s = makeWrappedSequenceWithEquatableElement([MinimalEquatableValue(1)])
  let separator = MinimalEquatableValue(1)
  expectCrashLater()
  _ = s.split(
    -1,
    allowEmptySlices: true) { extractValueFromEquatable($0) == separator }
}

self.test("\(testNamePrefix).split/semantics/separator/negativeMaxSplit") {
  let s = makeWrappedSequenceWithEquatableElement([MinimalEquatableValue(1)])
  let separator = wrapValueIntoEquatable(MinimalEquatableValue(1))
  expectCrashLater()
  _ = s.split(separator, maxSplit: -1, allowEmptySlices: true)
}

//===----------------------------------------------------------------------===//
// forEach()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).forEach/semantics") {
  let tests: [ForEachTest] = [
    ForEachTest([]),
    ForEachTest([1010]),
    ForEachTest([1010, 2020, 3030, 4040, 5050]),
  ]

  for test in tests {
    var elements: [Int] = []
    let closureLifetimeTracker = LifetimeTracked(0)
    let s = makeWrappedSequence(test.sequence.map(OpaqueValue.init))
    s.forEach {
      (element) in
      _blackHole(closureLifetimeTracker)
      elements.append(extractValue(element).value)
    }
    expectEqualSequence(
      test.sequence, elements,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
  } // addSequenceTests
}
