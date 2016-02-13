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

import StdlibUnittest

internal enum RangeSelection {
  case EmptyRange
  case LeftEdge
  case RightEdge
  case Middle
  case LeftHalf
  case RightHalf

  internal func rangeOf<
    C : Collection
  >(collection: C) -> Range<C.Index> {
    switch self {
      case .EmptyRange: return collection.endIndex..<collection.endIndex
      case .LeftEdge: return collection.startIndex..<collection.startIndex
      case .RightEdge: return collection.endIndex..<collection.endIndex
      case .Middle:
        let start = collection.startIndex.advanced(by: collection.count / 4)
        let end = collection.startIndex.advanced(by: 3 * collection.count / 4)
        return start...end
      case .LeftHalf:
        let start = collection.startIndex
        let end = start.advanced(by: collection.count / 2)
        return start..<end
      case .RightHalf:
        let start = collection.startIndex.advanced(by: collection.count / 2)
        let end = collection.endIndex
        return start..<end
    }
  }
}

internal enum IndexSelection {
  case Start
  case Middle
  case End
  case Last

  internal func indexIn<C : Collection>(collection: C) -> C.Index {
    switch self {
      case .Start: return collection.startIndex
      case .Middle: return collection.startIndex.advanced(by: collection.count / 2)
      case .End: return collection.endIndex
      case .Last: return collection.startIndex.advanced(by: collection.count - 1)
    }
  }
}

internal struct ReplaceRangeTest {
  let collection: [OpaqueValue<Int>]
  let newElements: [OpaqueValue<Int>]
  let rangeSelection: RangeSelection
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], newElements: [Int],
    rangeSelection: RangeSelection, expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.newElements = newElements.map(OpaqueValue.init)
    self.rangeSelection = rangeSelection
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "replaceSubrange() test data")
  }
}

internal struct AppendTest {
  let collection: [OpaqueValue<Int>]
  let newElement: OpaqueValue<Int>
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], newElement: Int, expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.newElement = OpaqueValue(newElement)
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "append() test data")
  }
}

internal struct AppendContentsOfTest {
  let collection: [OpaqueValue<Int>]
  let newElements: [OpaqueValue<Int>]
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], newElements: [Int], expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.newElements = newElements.map(OpaqueValue.init)
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "append() test data")
  }
}

internal struct InsertTest {
  let collection: [OpaqueValue<Int>]
  let newElement: OpaqueValue<Int>
  let indexSelection: IndexSelection
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], newElement: Int, indexSelection: IndexSelection,
    expected: [Int], file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.newElement = OpaqueValue(newElement)
    self.indexSelection = indexSelection
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "insert() test data")
  }
}

internal struct InsertContentsOfTest {
  let collection: [OpaqueValue<Int>]
  let newElements: [OpaqueValue<Int>]
  let indexSelection: IndexSelection
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], newElements: [Int], indexSelection: IndexSelection,
    expected: [Int], file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.newElements = newElements.map(OpaqueValue.init)
    self.indexSelection = indexSelection
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "insertContentsOf() test data")
  }
}

internal struct RemoveAtIndexTest {
  let collection: [OpaqueValue<Int>]
  let indexSelection: IndexSelection
  let expectedRemovedElement: Int
  let expectedCollection: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], indexSelection: IndexSelection,
    expectedRemovedElement: Int, expectedCollection: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.indexSelection = indexSelection
    self.expectedRemovedElement = expectedRemovedElement
    self.expectedCollection = expectedCollection
    self.loc = SourceLoc(file, line, comment: "remove(at:) test data")
  }
}

internal struct RemoveLastNTest {
  let collection: [OpaqueValue<Int>]
  let numberToRemove: Int
  let expectedCollection: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], numberToRemove: Int, expectedCollection: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.numberToRemove = numberToRemove
    self.expectedCollection = expectedCollection
    self.loc = SourceLoc(file, line, comment: "removeLast(n: Int) test data")
  }
}

internal struct RemoveRangeTest {
  let collection: [OpaqueValue<Int>]
  let rangeSelection: RangeSelection
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], rangeSelection: RangeSelection, expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.rangeSelection = rangeSelection
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "removeSubrange() test data")
  }
}

internal struct RemoveAllTest {
  let collection: [OpaqueValue<Int>]
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    collection: [Int], expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "removeAll() test data")
  }
}

internal struct ReserveCapacityTest {
  let collection: [OpaqueValue<Int>]
  let requestedCapacity: Int
  let loc: SourceLoc

  internal init(
    collection: [Int], requestedCapacity: Int,
    file: String = #file, line: UInt = #line
  ) {
    self.collection = collection.map(OpaqueValue.init)
    self.requestedCapacity = requestedCapacity
    self.loc = SourceLoc(file, line, comment: "removeAll() test data")
  }
}

internal struct OperatorPlusTest {
  let lhs: [OpaqueValue<Int>]
  let rhs: [OpaqueValue<Int>]
  let expected: [Int]
  let loc: SourceLoc

  internal init(
    lhs: [Int], rhs: [Int], expected: [Int],
    file: String = #file, line: UInt = #line
  ) {
    self.lhs = lhs.map(OpaqueValue.init)
    self.rhs = rhs.map(OpaqueValue.init)
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "`func +` test data")
  }
}

let removeLastTests: [RemoveLastNTest] = [
  RemoveLastNTest(
    collection: [1010],
    numberToRemove: 0,
    expectedCollection: [1010]
  ),
  RemoveLastNTest(
    collection: [1010],
    numberToRemove: 1,
    expectedCollection: []
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 0,
    expectedCollection: [1010, 2020, 3030, 4040, 5050]
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 1,
    expectedCollection: [1010, 2020, 3030, 4040]
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 2,
    expectedCollection: [1010, 2020, 3030]
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 3,
    expectedCollection: [1010, 2020]
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 4,
    expectedCollection: [1010]
  ),
  RemoveLastNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 5,
    expectedCollection: []
  ),
]

let appendContentsOfTests: [AppendContentsOfTest] = [
  AppendContentsOfTest(
    collection: [],
    newElements: [],
    expected: []),

  AppendContentsOfTest(
    collection: [1010],
    newElements: [],
    expected: [1010]),

  AppendContentsOfTest(
    collection: [1010, 2020, 3030, 4040],
    newElements: [],
    expected: [1010, 2020, 3030, 4040]),

  AppendContentsOfTest(
    collection: [],
    newElements: [1010],
    expected: [1010]),

  AppendContentsOfTest(
    collection: [1010],
    newElements: [2020],
    expected: [1010, 2020]),

  AppendContentsOfTest(
    collection: [1010],
    newElements: [2020, 3030, 4040],
    expected: [1010, 2020, 3030, 4040]),

  AppendContentsOfTest(
    collection: [1010, 2020, 3030, 4040],
    newElements: [5050, 6060, 7070, 8080],
    expected: [1010, 2020, 3030, 4040, 5050, 6060, 7070, 8080]),
]

extension TestSuite {
  /// Adds a set of tests for `RangeReplaceableCollection`.
  ///
  /// - parameter makeCollection: a factory function that creates a collection
  ///   instance with provided elements.
  ///
  ///   This facility can be used to test collection instances that can't be
  ///   constructed using APIs in the protocol (for example, `Array`s that wrap
  ///   `NSArray`s).
  public func addForwardRangeReplaceableCollectionTests<
    C : RangeReplaceableCollection,
    CollectionWithEquatableElement : RangeReplaceableCollection
    where
    C.SubSequence : Collection,
    C.SubSequence.Iterator.Element == C.Iterator.Element,
    C.SubSequence.SubSequence == C.SubSequence,
    CollectionWithEquatableElement.Iterator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([C.Iterator.Element]) -> C,
    wrapValue: (OpaqueValue<Int>) -> C.Iterator.Element,
    extractValue: (C.Iterator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Iterator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Iterator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Iterator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1
  ) {
    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(#function) {
      return
    }
    checksAdded.value.insert(#function)

    addForwardCollectionTests(
      testNamePrefix,
      makeCollection: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeCollectionOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks,
      outOfBoundsIndexOffset: outOfBoundsIndexOffset)

    func makeWrappedCollection(elements: [OpaqueValue<Int>]) -> C {
      return makeCollection(elements.map(wrapValue))
    }

    testNamePrefix += String(C.Type)

//===----------------------------------------------------------------------===//
// init()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).init()/semantics") {
  let c = C()
  expectEqualSequence([], c.map { extractValue($0).value })
}

//===----------------------------------------------------------------------===//
// init(Sequence)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).init(Sequence)/semantics") {
  for test in appendContentsOfTests {
    let c = C(test.newElements.map(wrapValue))
    expectEqualSequence(
      test.newElements.map { $0.value },
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// replaceSubrange()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).replaceSubrange()/semantics") {
  let tests: [ReplaceRangeTest] = [
    ReplaceRangeTest(
      collection: [],
      newElements: [],
      rangeSelection: .EmptyRange,
      expected: []),

    ReplaceRangeTest(
      collection: [],
      newElements: [1010, 2020, 3030],
      rangeSelection: .EmptyRange,
      expected: [1010, 2020, 3030]),

    ReplaceRangeTest(
      collection: [4040],
      newElements: [1010, 2020, 3030],
      rangeSelection: .LeftEdge,
      expected: [1010, 2020, 3030, 4040]),

    ReplaceRangeTest(
      collection: [1010],
      newElements: [2020, 3030, 4040],
      rangeSelection: .RightEdge,
      expected: [1010, 2020, 3030, 4040]),

    ReplaceRangeTest(
      collection: [1010, 2020, 3030],
      newElements: [4040],
      rangeSelection: .RightEdge,
      expected: [1010, 2020, 3030, 4040]),

    ReplaceRangeTest(
      collection: [1010, 2020, 3030, 4040, 5050],
      newElements: [9090],
      rangeSelection: .Middle,
      expected: [1010, 9090, 5050]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let rangeToReplace = test.rangeSelection.rangeOf(c)
    let newElements =
      MinimalForwardCollection(elements: test.newElements.map(wrapValue))
    c.replaceSubrange(rangeToReplace, with: newElements)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// append()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).append()/semantics") {
  let tests: [AppendTest] = [
    AppendTest(
      collection: [],
      newElement: 1010,
      expected: [1010]),

    AppendTest(
      collection: [1010],
      newElement: 2020,
      expected: [1010, 2020]),

    AppendTest(
      collection: [1010, 2020, 3030, 4040, 5050, 6060, 7070],
      newElement: 8080,
      expected: [1010, 2020, 3030, 4040, 5050, 6060, 7070, 8080]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let newElement = wrapValue(test.newElement)
    c.append(newElement)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// appendContentsOf()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).appendContentsOf()/semantics") {
  for test in appendContentsOfTests {
    var c = makeWrappedCollection(test.collection)
    let newElements =
      MinimalForwardCollection(elements: test.newElements.map(wrapValue))
    c.appendContentsOf(newElements)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// insert()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).insert()/semantics") {
  let tests: [InsertTest] = [
    InsertTest(
      collection: [],
      newElement: 1010,
      indexSelection: IndexSelection.Start,
      expected: [1010]),

    InsertTest(
      collection: [2020],
      newElement: 1010,
      indexSelection: .Start,
      expected: [1010, 2020]),

    InsertTest(
      collection: [1010],
      newElement: 2020,
      indexSelection: .End,
      expected: [1010, 2020]),

    InsertTest(
      collection: [2020, 3030, 4040, 5050],
      newElement: 1010,
      indexSelection: .Start,
      expected: [1010, 2020, 3030, 4040, 5050]),

    InsertTest(
      collection: [1010, 2020, 3030, 4040],
      newElement: 5050,
      indexSelection: .End,
      expected: [1010, 2020, 3030, 4040, 5050]),

    InsertTest(
      collection: [1010, 2020, 4040, 5050],
      newElement: 3030,
      indexSelection: .Middle,
      expected: [1010, 2020, 3030, 4040, 5050]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let newElement = wrapValue(test.newElement)
    c.insert(newElement, at: test.indexSelection.indexIn(c))
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// insertContentsOf()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).insertContentsOf()/semantics") {
  let tests: [InsertContentsOfTest] = [
    InsertContentsOfTest(
      collection: [],
      newElements: [],
      indexSelection: IndexSelection.Start,
      expected: []),

    InsertContentsOfTest(
      collection: [],
      newElements: [1010],
      indexSelection: .Start,
      expected: [1010]),

    InsertContentsOfTest(
      collection: [],
      newElements: [1010, 2020, 3030, 4040],
      indexSelection: .Start,
      expected: [1010, 2020, 3030, 4040]),

    InsertContentsOfTest(
      collection: [2020],
      newElements: [1010],
      indexSelection: .Start,
      expected: [1010, 2020]),

    InsertContentsOfTest(
      collection: [1010],
      newElements: [2020],
      indexSelection: .End,
      expected: [1010, 2020]),

    InsertContentsOfTest(
      collection: [4040],
      newElements: [1010, 2020, 3030],
      indexSelection: .Start,
      expected: [1010, 2020, 3030, 4040]),

    InsertContentsOfTest(
      collection: [1010],
      newElements: [2020, 3030, 4040],
      indexSelection: .End,
      expected: [1010, 2020, 3030, 4040]),

    InsertContentsOfTest(
      collection: [1010, 2020, 4040, 5050],
      newElements: [3030],
      indexSelection: .Middle,
      expected: [1010, 2020, 3030, 4040, 5050]),

    InsertContentsOfTest(
      collection: [4040, 5050, 6060],
      newElements: [1010, 2020, 3030],
      indexSelection: .Start,
      expected: [1010, 2020, 3030, 4040, 5050, 6060]),

    InsertContentsOfTest(
      collection: [1010, 2020, 3030],
      newElements: [4040, 5050, 6060],
      indexSelection: .End,
      expected: [1010, 2020, 3030, 4040, 5050, 6060]),

    InsertContentsOfTest(
      collection: [1010, 2020, 3030, 7070, 8080, 9090],
      newElements: [4040, 5050, 6060],
      indexSelection: .Middle,
      expected: [1010, 2020, 3030, 4040, 5050, 6060, 7070, 8080, 9090]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let newElements =
      MinimalForwardCollection(elements: test.newElements.map(wrapValue))
    c.insertContentsOf(newElements, at: test.indexSelection.indexIn(c))
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// remove(at:)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).remove(at:)/semantics") {
  let tests: [RemoveAtIndexTest] = [
    RemoveAtIndexTest(
      collection: [1010],
      indexSelection: .Start,
      expectedRemovedElement: 1010,
      expectedCollection: []),

    RemoveAtIndexTest(
      collection: [1010, 2020, 3030],
      indexSelection: .Start,
      expectedRemovedElement: 1010,
      expectedCollection: [2020, 3030]),

    RemoveAtIndexTest(
      collection: [1010, 2020, 3030],
      indexSelection: .Middle,
      expectedRemovedElement: 2020,
      expectedCollection: [1010, 3030]),

    RemoveAtIndexTest(
      collection: [1010, 2020, 3030],
      indexSelection: .Last,
      expectedRemovedElement: 3030,
      expectedCollection: [1010, 2020]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let removedElement = c.remove(at: test.indexSelection.indexIn(c))
    expectEqualSequence(
      test.expectedCollection,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqual(
      test.expectedRemovedElement,
      extractValue(removedElement).value,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// removeFirst()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeFirst()/semantics") {
  for test in removeFirstTests.filter({ $0.numberToRemove == 1 }) {
    var c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    let removedElement = c.removeFirst()
    expectEqual(test.collection.first, extractValue(removedElement).value)
    expectEqualSequence(
      test.expectedCollection,
      c.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the tail of the collection",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeFirst()/empty/semantics") {
  var c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  expectCrashLater()
  _ = c.removeFirst() // Should trap.
}

//===----------------------------------------------------------------------===//
// removeFirst(n: Int)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeFirst(n: Int)/semantics") {
  for test in removeFirstTests {
    var c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    c.removeFirst(test.numberToRemove)
    expectEqualSequence(
      test.expectedCollection,
      c.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the tail of the collection",
      stackTrace: SourceLocStack().with(test.loc)
    )
  }
}

self.test("\(testNamePrefix).removeFirst(n: Int)/empty/semantics") {
  var c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  expectCrashLater()
  c.removeFirst(1) // Should trap.
}

self.test("\(testNamePrefix).removeFirst(n: Int)/removeNegative/semantics") {
  var c = makeWrappedCollection([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  c.removeFirst(-1) // Should trap.
}

self.test("\(testNamePrefix).removeFirst(n: Int)/removeTooMany/semantics") {
  var c = makeWrappedCollection([1010, 2020, 3030].map(OpaqueValue.init))
  expectCrashLater()
  c.removeFirst(5) // Should trap.
}

//===----------------------------------------------------------------------===//
// removeSubrange()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeSubrange()/semantics") {
  let tests: [RemoveRangeTest] = [
    RemoveRangeTest(
      collection: [],
      rangeSelection: .EmptyRange,
      expected: []),

    RemoveRangeTest(
      collection: [1010],
      rangeSelection: .Middle,
      expected: []),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040],
      rangeSelection: .LeftHalf,
      expected: [3030, 4040]),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040],
      rangeSelection: .RightHalf,
      expected: [1010, 2020]),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040, 5050],
      rangeSelection: .Middle,
      expected: [1010, 5050]),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040, 5050, 6060],
      rangeSelection: .LeftHalf,
      expected: [4040, 5050, 6060]),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040, 5050, 6060],
      rangeSelection: .RightHalf,
      expected: [1010, 2020, 3030]),

    RemoveRangeTest(
      collection: [1010, 2020, 3030, 4040, 5050, 6060],
      rangeSelection: .Middle,
      expected: [1010, 6060]),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    let rangeToRemove = test.rangeSelection.rangeOf(c)
    c.removeSubrange(rangeToRemove)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// removeAll()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeAll()/semantics") {
  let tests: [RemoveAllTest] = [
    RemoveAllTest(
      collection: [],
      expected: []),

    RemoveAllTest(
      collection: [1010],
      expected: []),

    RemoveAllTest(
      collection: [1010, 2020, 3030, 4040, 5050],
      expected: []),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    c.removeAll()
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    c.removeAll(keepingCapacity: false)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    c.removeAll(keepingCapacity: true)
    expectEqualSequence(
      test.expected,
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// reserveCapacity()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).reserveCapacity()/semantics") {
  let tests: [ReserveCapacityTest] = [
    ReserveCapacityTest(
      collection: [],
      requestedCapacity: 0),

    ReserveCapacityTest(
      collection: [],
      requestedCapacity: 3),

    ReserveCapacityTest(
      collection: [],
      requestedCapacity: 100),

    ReserveCapacityTest(
      collection: [ 1010 ],
      requestedCapacity: 0),

    ReserveCapacityTest(
      collection: [ 1010 ],
      requestedCapacity: 3),

    ReserveCapacityTest(
      collection: [ 1010 ],
      requestedCapacity: 100),

    ReserveCapacityTest(
      collection: [ 1010, 2020, 3030 ],
      requestedCapacity: 0),

    ReserveCapacityTest(
      collection: [ 1010, 2020, 3030 ],
      requestedCapacity: 3),

    ReserveCapacityTest(
      collection: [ 1010, 2020, 3030 ],
      requestedCapacity: 100),
  ]

  for test in tests {
    var c = makeWrappedCollection(test.collection)
    c.reserveCapacity(numericCast(test.requestedCapacity))
    expectEqualSequence(
      test.collection.map { $0.value },
      c.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// + operator
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).OperatorPlus") {
  let tests: [OperatorPlusTest] = [
    OperatorPlusTest(
      lhs: [],
      rhs: [],
      expected: []),

    OperatorPlusTest(
      lhs: [],
      rhs: [ 1010 ],
      expected: [ 1010 ]),

    OperatorPlusTest(
      lhs: [ 1010 ],
      rhs: [],
      expected: [ 1010 ]),

    OperatorPlusTest(
      lhs: [ 1010 ],
      rhs: [ 2020 ],
      expected: [ 1010, 2020 ]),

    OperatorPlusTest(
      lhs: [ 1010, 2020, 3030 ],
      rhs: [],
      expected: [ 1010, 2020, 3030 ]),

    OperatorPlusTest(
      lhs: [],
      rhs: [ 1010, 2020, 3030 ],
      expected: [ 1010, 2020, 3030 ]),

    OperatorPlusTest(
      lhs: [ 1010 ],
      rhs: [ 2020, 3030, 4040 ],
      expected: [ 1010, 2020, 3030, 4040 ]),

    OperatorPlusTest(
      lhs: [ 1010, 2020, 3030 ],
      rhs: [ 4040 ],
      expected: [ 1010, 2020, 3030, 4040 ]),

    OperatorPlusTest(
      lhs: [ 1010, 2020, 3030, 4040 ],
      rhs: [ 5050, 6060, 7070 ],
      expected: [ 1010, 2020, 3030, 4040, 5050, 6060, 7070 ]),

    OperatorPlusTest(
      lhs: [ 1010, 2020, 3030 ],
      rhs: [ 4040, 5050, 6060, 7070 ],
      expected: [ 1010, 2020, 3030, 4040, 5050, 6060, 7070 ]),
  ]

  // RangeReplaceableCollection + Sequence
  for test in tests {
    let lhs = makeWrappedCollection(test.lhs)
    let rhs = MinimalSequence(elements: test.rhs.map(wrapValue))

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.lhs.map { $0.value },
      lhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  // Sequence + RangeReplaceableCollection
  for test in tests {
    let lhs = MinimalSequence(elements: test.lhs.map(wrapValue))
    let rhs = makeWrappedCollection(test.rhs)

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.rhs.map { $0.value },
      rhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  // RangeReplaceableCollection + Collection
  for test in tests {
    let lhs = makeWrappedCollection(test.lhs)
    let rhs = MinimalForwardCollection(elements: test.rhs.map(wrapValue))

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.lhs.map { $0.value },
      lhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqualSequence(
      test.rhs.map { $0.value },
      rhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  // RangeReplaceableCollection + same RangeReplaceableCollection
  for test in tests {
    let lhs = makeWrappedCollection(test.lhs)
    let rhs = makeWrappedCollection(test.rhs)

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.lhs.map { $0.value },
      lhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqualSequence(
      test.rhs.map { $0.value },
      rhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  // RangeReplaceableCollection + MinimalForwardRangeReplaceableCollection
  for test in tests {
    let lhs = makeWrappedCollection(test.lhs)
    let rhs = MinimalForwardRangeReplaceableCollection(
      elements: test.rhs.map(wrapValue))

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.lhs.map { $0.value },
      lhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqualSequence(
      test.rhs.map { $0.value },
      rhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }

  // MinimalForwardRangeReplaceableCollection + RangeReplaceableCollection
  for test in tests {
    let lhs = MinimalForwardRangeReplaceableCollection(
      elements: test.lhs.map(wrapValue))
    let rhs = makeWrappedCollection(test.rhs)

    let result = lhs + rhs
    expectEqualSequence(
      test.expected,
      result.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))

    expectEqualSequence(
      test.lhs.map { $0.value },
      lhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqualSequence(
      test.rhs.map { $0.value },
      rhs.map { extractValue($0).value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//

  } // addForwardRangeReplaceableCollectionTests

  public func addBidirectionalRangeReplaceableCollectionTests<
    C : RangeReplaceableCollection,
    CollectionWithEquatableElement : RangeReplaceableCollection
    where
    C.Index : BidirectionalIndex,
    C.SubSequence : Collection,
    C.SubSequence.Iterator.Element == C.Iterator.Element,
    C.SubSequence.Index : BidirectionalIndex,
    C.SubSequence.SubSequence == C.SubSequence,
    CollectionWithEquatableElement.Index : BidirectionalIndex,
    CollectionWithEquatableElement.Iterator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([C.Iterator.Element]) -> C,
    wrapValue: (OpaqueValue<Int>) -> C.Iterator.Element,
    extractValue: (C.Iterator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Iterator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Iterator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Iterator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1
  ) {
    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(#function) {
      return
    }
    checksAdded.value.insert(#function)

    addForwardRangeReplaceableCollectionTests(
      testNamePrefix,
      makeCollection: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeCollectionOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks,
      outOfBoundsIndexOffset: outOfBoundsIndexOffset)

    addBidirectionalCollectionTests(
      testNamePrefix,
      makeCollection: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeCollectionOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks,
      outOfBoundsIndexOffset: outOfBoundsIndexOffset)

    func makeWrappedCollection(elements: [OpaqueValue<Int>]) -> C {
      return makeCollection(elements.map(wrapValue))
    }

    testNamePrefix += String(C.Type)

//===----------------------------------------------------------------------===//
// removeLast()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeLast()/whereIndexIsBidirectional/semantics") {
  for test in removeLastTests.filter({ $0.numberToRemove == 1 }) {
    var c = makeWrappedCollection(test.collection)
    let removedElement = c.removeLast()
    expectEqual(
      test.collection.last!.value,
      extractValue(removedElement).value,
      stackTrace: SourceLocStack().with(test.loc))
    expectEqualSequence(
      test.expectedCollection,
      c.map { extractValue($0).value },
      "removeLast() shouldn't mutate the head of the collection",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeLast()/whereIndexIsBidirectional/empty/semantics") {
  var c = makeWrappedCollection([])
  expectCrashLater()
  c.removeLast() // Should trap.
}

//===----------------------------------------------------------------------===//
// removeLast(n: Int)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeLast(n: Int)/whereIndexIsBidirectional/semantics") {
  for test in removeLastTests {
    var c = makeWrappedCollection(test.collection)
    c.removeLast(test.numberToRemove)
    expectEqualSequence(
      test.expectedCollection,
      c.map { extractValue($0).value },
      "removeLast() shouldn't mutate the head of the collection",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeLast(n: Int)/whereIndexIsBidirectional/empty/semantics") {
  var c = makeWrappedCollection([])
  expectCrashLater()
  c.removeLast(1) // Should trap.
}

self.test("\(testNamePrefix).removeLast(n: Int)/whereIndexIsBidirectional/removeNegative/semantics") {
  var c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  expectCrashLater()
  c.removeLast(-1) // Should trap.
}

self.test("\(testNamePrefix).removeLast(n: Int)/whereIndexIsBidirectional/removeTooMany/semantics") {
  var c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  expectCrashLater()
  c.removeLast(3) // Should trap.
}

//===----------------------------------------------------------------------===//

  } // addBidirectionalRangeReplaceableCollectionTests

  public func addRandomAccessRangeReplaceableCollectionTests<
    C : RangeReplaceableCollection,
    CollectionWithEquatableElement : RangeReplaceableCollection
    where
    C.Index : RandomAccessIndex,
    C.SubSequence : Collection,
    C.SubSequence.Iterator.Element == C.Iterator.Element,
    C.SubSequence.Index : RandomAccessIndex,
    C.SubSequence.SubSequence == C.SubSequence,
    CollectionWithEquatableElement.Index : RandomAccessIndex,
    CollectionWithEquatableElement.Iterator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([C.Iterator.Element]) -> C,
    wrapValue: (OpaqueValue<Int>) -> C.Iterator.Element,
    extractValue: (C.Iterator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Iterator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Iterator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Iterator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1
  ) {
    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(#function) {
      return
    }
    checksAdded.value.insert(#function)

    addBidirectionalRangeReplaceableCollectionTests(
      testNamePrefix,
      makeCollection: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeCollectionOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks,
      outOfBoundsIndexOffset: outOfBoundsIndexOffset)

    addRandomAccessCollectionTests(
      testNamePrefix,
      makeCollection: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeCollectionOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks,
      outOfBoundsIndexOffset: outOfBoundsIndexOffset)

    testNamePrefix += String(C.Type)

    // No extra checks for collections with random access traversal so far.
  } // addRandomAccessRangeReplaceableCollectionTests
}

