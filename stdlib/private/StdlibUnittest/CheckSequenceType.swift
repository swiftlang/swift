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
    self.sequence = sequence.map { MinimalEquatableValue($0) }
    self.expectedLeftoverSequence = expectedLeftoverSequence.map {
      MinimalEquatableValue($0)}
    self.loc = SourceLoc(file, line, comment: "test data")
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

extension TestSuite {
  public func addSequenceTests<
    Sequence : SequenceType,
    SequenceWithEquatableElement : SequenceType
    where
    SequenceWithEquatableElement.Generator.Element : Equatable
  >(
    var testNamePrefix: String = "",
    makeSequence: ([Sequence.Generator.Element]) -> Sequence,
    wrapValue: (OpaqueValue<Int>) -> Sequence.Generator.Element,
    extractValue: (Sequence.Generator.Element) -> OpaqueValue<Int>,

    makeSequenceOfEquatable: ([SequenceWithEquatableElement.Generator.Element]) -> SequenceWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> SequenceWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((SequenceWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([])
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

  }
}

