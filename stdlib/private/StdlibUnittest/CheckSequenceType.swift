//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct FindTest {
  public let expected: Int?
  public let element: MinimalEquatableValue
  public let sequence: [MinimalEquatableValue]
  public let expectedLeftoverSequence: [MinimalEquatableValue]
  public let loc: SourceLoc

  public init(
    expected: Int?, element: Int, sequence: [Int],
    expectedLeftoverSequence: [Int],
    file: String = __FILE__, line: UWord = __LINE__
  ) {
    self.expected = expected
    self.element = MinimalEquatableValue(element)
    self.sequence = sequence.map(MinimalEquatableValue.init)
    self.expectedLeftoverSequence = expectedLeftoverSequence.map(
      MinimalEquatableValue.init)
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct DropFirstTest {
  public var sequence: [Int]
  public let dropElements: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], dropElements: Int, expected: [Int],
      file: String = __FILE__, line: UWord = __LINE__) {
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
      file: String = __FILE__, line: UWord = __LINE__) {
    self.sequence = sequence
    self.dropElements = dropElements
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "dropLast() test data")
  }
}

public struct PrefixTest {
  public var sequence: [Int]
  public let maxLength: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], maxLength: Int, expected: [Int],
      file: String = __FILE__, line: UWord = __LINE__) {
    self.sequence = sequence
    self.maxLength = maxLength
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "prefix() test data")
  }
}

public struct SuffixTest {
  public var sequence: [Int]
  public let maxLength: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(sequence: [Int], maxLength: Int, expected: [Int],
      file: String = __FILE__, line: UWord = __LINE__) {
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
       allowEmptySlices: Bool, file: String = __FILE__, line: UWord = __LINE__) {
    self.sequence = sequence
    self.maxSplit = maxSplit
    self.separator = separator
    self.allowEmptySlices = allowEmptySlices
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "suffix() test data")
  }
}

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

let splitTests: [SplitTest] = [
  SplitTest(
    sequence: [],
    maxSplit: Int.max,
    separator: 1,
    expected: [[]],
    allowEmptySlices: true
  ),
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
    separator: 1,
    expected: [],
    allowEmptySlices: false
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
    maxSplit: Int.max,
    separator: 1,
    expected: [],
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
    expected: [[1], [], []],
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

/// This test performs a side effect on each element of `sequence`. The expected
/// side effect for the purposes of testing is to append each element to an
/// external array, which can be compared to `sequence`.
internal struct ForEachTest {
  let sequence: [Int]
  let loc: SourceLoc

  init(
    _ sequence: [Int],
    file: String = __FILE__, line: UWord = __LINE__
  ) {
    self.sequence = sequence
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

extension TestSuite {
  public func addSequenceTests<
    Sequence : SequenceType,
    SequenceWithEquatableElement : SequenceType
    where
    SequenceWithEquatableElement.Generator.Element : Equatable,
    Sequence.SubSequence : SequenceType,
    Sequence.Generator.Element == Sequence.SubSequence.Generator.Element,
    Sequence.SubSequence.SubSequence : SequenceType,
    Sequence.SubSequence == Sequence.SubSequence.SubSequence
  >(
    var testNamePrefix: String = "",
    makeSequence: ([Sequence.Generator.Element]) -> Sequence,
    wrapValue: (OpaqueValue<Int>) -> Sequence.Generator.Element,
    extractValue: (Sequence.Generator.Element) -> OpaqueValue<Int>,

    makeSequenceOfEquatable: ([SequenceWithEquatableElement.Generator.Element]) -> SequenceWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> SequenceWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((SequenceWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all
  ) {

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

    let result1 = s1.dropFirst(1).dropFirst(1)
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
  s.dropFirst(-1)
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
  s.dropLast(-1)
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
  let prefixedTwice = s2.prefix(3).prefix(3)

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
  let prefixedTwice = s2.suffix(3).prefix(3)

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

self.test("\(testNamePrefix).split/semantics/negativeMaxSplit") {
  expectCrashLater()
  let s = makeWrappedSequenceWithEquatableElement([MinimalEquatableValue(1)])
  let separator = MinimalEquatableValue(1)
  _ = s.split(
    -1,
    allowEmptySlices: true) { extractValueFromEquatable($0) == separator }
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
