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

public struct SubscriptRangeTest {
  public let expected: [OpaqueValue<Int>]
  public let collection: [OpaqueValue<Int>]
  public let bounds: Range<Int>
  public let count: Int
  public let loc: SourceLoc

  public var isEmpty: Bool { return count == 0 }

  public func boundsIn<C : CollectionType>(c: C) -> Range<C.Index> {
    let i = c.startIndex
    return Range(
      start: i.advancedBy(numericCast(bounds.startIndex)),
      end: i.advancedBy(numericCast(bounds.endIndex)))
  }

  public init(
    expected: [Int], collection: [Int], bounds: Range<Int>,
    count: Int,
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.expected = expected.map(OpaqueValue.init)
    self.collection = collection.map(OpaqueValue.init)
    self.bounds = bounds
    self.count = count
    self.loc = SourceLoc(file, line, comment: "test data")
  }
}

public struct PrefixThroughTest {
  public var collection: [Int]
  public let position: Int
  public let expected: [Int]
  public let loc: SourceLoc

  init(
    collection: [Int], position: Int, expected: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.collection = collection
    self.position = position
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "prefix() test data")
  }
}

public struct PrefixUpToTest {
  public var collection: [Int]
  public let end: Int
  public let expected: [Int]
  public let loc: SourceLoc

  public init(
    collection: [Int], end: Int, expected: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.collection = collection
    self.end = end
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "prefix() test data")
  }
}

internal struct RemoveFirstNTest {
  let collection: [Int]
  let numberToRemove: Int
  let expectedCollection: [Int]
  let loc: SourceLoc

  init(
    collection: [Int], numberToRemove: Int, expectedCollection: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.collection = collection
    self.numberToRemove = numberToRemove
    self.expectedCollection = expectedCollection
    self.loc = SourceLoc(file, line, comment: "removeFirst(n: Int) test data")
  }
}

public struct SuffixFromTest {
  public var collection: [Int]
  public let start: Int
  public let expected: [Int]
  public let loc: SourceLoc

  init(
    collection: [Int], start: Int, expected: [Int],
    file: String = __FILE__, line: UInt = __LINE__
  ) {
    self.collection = collection
    self.start = start
    self.expected = expected
    self.loc = SourceLoc(file, line, comment: "prefix() test data")
  }
}

public let subscriptRangeTests = [
  // Slice an empty collection.
  SubscriptRangeTest(
    expected: [],
    collection: [],
    bounds: 0..<0,
    count: 0),

  // Slice to the full extent.
  SubscriptRangeTest(
    expected: [ 1010 ],
    collection: [ 1010 ],
    bounds: 0..<1,
    count: 1),
  SubscriptRangeTest(
    expected: [ 1010, 2020, 3030 ],
    collection: [ 1010, 2020, 3030 ],
    bounds: 0..<3,
    count: 3),

  // Slice an empty prefix.
  SubscriptRangeTest(
    expected: [],
    collection: [ 1010, 2020, 3030 ],
    bounds: 0..<0,
    count: 3),

  // Slice a prefix.
  SubscriptRangeTest(
    expected: [ 1010, 2020 ],
    collection: [ 1010, 2020, 3030 ],
    bounds: 0..<2,
    count: 3),

  // Slice an empty suffix.
  SubscriptRangeTest(
    expected: [],
    collection: [ 1010, 2020, 3030 ],
    bounds: 3..<3,
    count: 3),

  // Slice a suffix.
  SubscriptRangeTest(
    expected: [ 2020, 3030 ],
    collection: [ 1010, 2020, 3030 ],
    bounds: 1..<3,
    count: 3),

  // Slice an empty range in the middle.
  SubscriptRangeTest(
    expected: [],
    collection: [ 1010, 2020, 3030 ],
    bounds: 2..<2,
    count: 3),

  // Slice the middle part.
  SubscriptRangeTest(
    expected: [ 2020 ],
    collection: [ 1010, 2020, 3030 ],
    bounds: 1..<2,
    count: 3),
  SubscriptRangeTest(
    expected: [ 2020, 3030, 4040 ],
    collection: [ 1010, 2020, 3030, 4040, 5050, 6060 ],
    bounds: 1..<4,
    count: 6),
]

public let prefixUpToTests = [
  PrefixUpToTest(
    collection: [],
    end: 0,
    expected: []
  ),
  PrefixUpToTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    end: 3,
    expected: [1010, 2020, 3030]
  ),
  PrefixUpToTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    end: 5,
    expected: [1010, 2020, 3030, 4040, 5050]
  ),
]

public let prefixThroughTests = [
  PrefixThroughTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    position: 0,
    expected: [1010]
  ),
  PrefixThroughTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    position: 2,
    expected: [1010, 2020, 3030]
  ),
  PrefixThroughTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    position: 4,
    expected: [1010, 2020, 3030, 4040, 5050]
  ),
]

public let suffixFromTests = [
  SuffixFromTest(
    collection: [],
    start: 0,
    expected: []
  ),
  SuffixFromTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    start: 0,
    expected: [1010, 2020, 3030, 4040, 5050]
  ),
  SuffixFromTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    start: 3,
    expected: [4040, 5050]
  ),
  SuffixFromTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    start: 5,
    expected: []
  ),
]

let removeFirstTests: [RemoveFirstNTest] = [
  RemoveFirstNTest(
    collection: [1010],
    numberToRemove: 0,
    expectedCollection: [1010]
  ),
  RemoveFirstNTest(
    collection: [1010],
    numberToRemove: 1,
    expectedCollection: []
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 0,
    expectedCollection: [1010, 2020, 3030, 4040, 5050]
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 1,
    expectedCollection: [2020, 3030, 4040, 5050]
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 2,
    expectedCollection: [3030, 4040, 5050]
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 3,
    expectedCollection: [4040, 5050]
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 4,
    expectedCollection: [5050]
  ),
  RemoveFirstNTest(
    collection: [1010, 2020, 3030, 4040, 5050],
    numberToRemove: 5,
    expectedCollection: []
  ),
]

extension TestSuite {
  public func addForwardCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    Collection.SubSequence.SubSequence == Collection.SubSequence,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1,
    outOfBoundsSubscriptOffset: Int = 1
  ) {

    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(__FUNCTION__) {
      return
    }
    checksAdded.value.insert(__FUNCTION__)

    addSequenceTests(
      testNamePrefix,
      makeSequence: makeCollection,
      wrapValue: wrapValue,
      extractValue: extractValue,
      makeSequenceOfEquatable: makeCollectionOfEquatable,
      wrapValueIntoEquatable: wrapValueIntoEquatable,
      extractValueFromEquatable: extractValueFromEquatable,
      checksAdded: checksAdded,
      resiliencyChecks: resiliencyChecks)

    func makeWrappedCollection(elements: [OpaqueValue<Int>]) -> Collection {
      return makeCollection(elements.map(wrapValue))
    }

    func makeWrappedCollectionWithEquatableElement(
      elements: [MinimalEquatableValue]
    ) -> CollectionWithEquatableElement {
      return makeCollectionOfEquatable(elements.map(wrapValueIntoEquatable))
    }

    testNamePrefix += String(Collection.Type)

//===----------------------------------------------------------------------===//
// generate()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).generate()/semantics") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    for _ in 0..<3 {
      checkSequence(
        test.collection.map(wrapValue),
        c,
        resiliencyChecks: .none) {
        extractValue($0).value == extractValue($1).value
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// Index
//===----------------------------------------------------------------------===//

if resiliencyChecks.creatingOutOfBoundsIndicesBehavior != .None {
  self.test("\(testNamePrefix).Index/OutOfBounds/Right/NonEmpty") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    let index = c.endIndex
    if resiliencyChecks.creatingOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      _blackHole(index.advancedBy(numericCast(outOfBoundsIndexOffset)))
    } else {
      expectFailure {
        _blackHole(index.advancedBy(numericCast(outOfBoundsIndexOffset)))
      }
    }
  }

  self.test("\(testNamePrefix).Index/OutOfBounds/Right/Empty") {
    let c = makeWrappedCollection([])
    let index = c.endIndex
    if resiliencyChecks.creatingOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      _blackHole(index.advancedBy(numericCast(outOfBoundsIndexOffset)))
    } else {
      expectFailure {
        _blackHole(index.advancedBy(numericCast(outOfBoundsIndexOffset)))
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// subscript(_: Index)
//===----------------------------------------------------------------------===//

if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior != .None {
  self.test("\(testNamePrefix).subscript(_: Index)/OutOfBounds/Right/NonEmpty/Get") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    var index = c.endIndex
    if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
      _blackHole(c[index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
        _blackHole(c[index])
      }
    }
  }

  self.test("\(testNamePrefix).subscript(_: Index)/OutOfBounds/Right/Empty/Get") {
    let c = makeWrappedCollection([])
    var index = c.endIndex
    if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
      _blackHole(c[index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
        _blackHole(c[index])
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// subscript(_: Range)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).subscript(_: Range)/Get/semantics") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    let result = c[test.boundsIn(c)]

    // FIXME: improve checkForwardCollection to check the SubSequence type.
    checkForwardCollection(
      test.expected.map(wrapValue),
      result,
      resiliencyChecks: .none) {
      extractValue($0).value == extractValue($1).value
    }
  }
}

if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior != .None {
  self.test("\(testNamePrefix).subscript(_: Range)/OutOfBounds/Right/NonEmpty/Get") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    var index = c.endIndex
    if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
      _blackHole(c[index..<index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
        _blackHole(c[index..<index])
      }
    }
  }

  self.test("\(testNamePrefix).subscript(_: Range)/OutOfBounds/Right/Empty/Get") {
    let c = makeWrappedCollection([])
    var index = c.endIndex
    if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
      _blackHole(c[index..<index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(outOfBoundsSubscriptOffset))
        _blackHole(c[index..<index])
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// isEmpty
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).isEmpty/semantics") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    expectEqual(test.isEmpty, c.isEmpty)
  }
}

//===----------------------------------------------------------------------===//
// count
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).count/semantics") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    expectEqual(test.count, numericCast(c.count) as Int)
  }
}

//===----------------------------------------------------------------------===//
// indexOf()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).indexOf()/WhereElementIsEquatable/semantics") {
  for test in findTests {
    let c = makeWrappedCollectionWithEquatableElement(test.sequence)
    var result = c.indexOf(wrapValueIntoEquatable(test.element))
    expectType(
      Optional<CollectionWithEquatableElement.Index>.self,
      &result)
    let zeroBasedIndex = result.map {
      numericCast(c.startIndex.distanceTo($0)) as Int
    }
    expectEqual(
      test.expected,
      zeroBasedIndex,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).indexOf()/Predicate/semantics") {
  for test in findTests {
    let c = makeWrappedCollectionWithEquatableElement(test.sequence)
    let closureLifetimeTracker = LifetimeTracked(0)
    expectEqual(1, LifetimeTracked.instances)
    let result = c.indexOf {
      (candidate) in
      _blackHole(closureLifetimeTracker)
      return extractValueFromEquatable(candidate).value == test.element.value
    }
    let zeroBasedIndex = result.map {
      numericCast(c.startIndex.distanceTo($0)) as Int
    }
    expectEqual(
      test.expected,
      zeroBasedIndex,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// first
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).first") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    let result = c.first
    if test.isEmpty {
      expectEmpty(result)
    } else {
      expectOptionalEqual(
        test.collection[0],
        result.map(extractValue)
      ) { $0.value == $1.value }
    }
  }
}

//===----------------------------------------------------------------------===//
// indices
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).indices") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    let indices = c.indices
    expectEqual(c.startIndex, indices.startIndex)
    expectEqual(c.endIndex, indices.endIndex)
  }
}

//===----------------------------------------------------------------------===//
// dropFirst()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).dropFirst/semantics") {
  for test in dropFirstTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.dropFirst(test.dropElements)
    expectEqualSequence(
      test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// dropLast()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).dropLast/semantics") {
  for test in dropLastTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.dropLast(test.dropElements)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// prefix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).prefix/semantics") {
  for test in prefixTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.prefix(test.maxLength)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// suffix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).suffix/semantics") {
  for test in suffixTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.suffix(test.maxLength)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// split()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).split/semantics") {
  for test in splitTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.split(test.maxSplit,
        allowEmptySlices: test.allowEmptySlices) {
      extractValue($0).value == test.separator
    }
    expectEqualSequence(test.expected, result.map {
      $0.map {
        extractValue($0).value
      }
    },
    stackTrace: SourceLocStack().with(test.loc)) { $0 == $1 }
  }
}

//===----------------------------------------------------------------------===//
// prefixThrough()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).prefixThrough/semantics") {
  for test in prefixThroughTests {
    let c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    let index = c.startIndex.advancedBy(numericCast(test.position))
    let result = c.prefixThrough(index)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// prefixUpTo()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).prefixUpTo/semantics") {
  for test in prefixUpToTests {
    let c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    let index = c.startIndex.advancedBy(numericCast(test.end))
    let result = c.prefixUpTo(index)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// suffixFrom()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).suffixFrom/semantics") {
  for test in suffixFromTests {
    let c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    let index = c.startIndex.advancedBy(numericCast(test.start))
    let result = c.suffixFrom(index)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// removeFirst()/slice
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeFirst()/slice/semantics") {
  for test in removeFirstTests.filter({ $0.numberToRemove == 1 }) {
    let c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    var slice = c[c.startIndex..<c.endIndex]
    let survivingIndices = Array(slice.startIndex.successor()..<slice.endIndex)
    let removedElement = slice.removeFirst()
    expectEqual(test.collection.first, extractValue(removedElement).value)
    expectEqualSequence(
      test.expectedCollection,
      slice.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the tail of the slice",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.expectedCollection,
      survivingIndices.map { extractValue(slice[$0]).value },
      "removeFirst() shouldn't invalidate indices",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.collection,
      c.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the collection that was sliced",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeFirst()/slice/empty/semantics") {
  let c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  _ = slice.removeFirst() // Should trap.
}

//===----------------------------------------------------------------------===//
// removeFirst(n: Int)/slice
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeFirst(n: Int)/slice/semantics") {
  for test in removeFirstTests {
    let c = makeWrappedCollection(test.collection.map(OpaqueValue.init))
    var slice = c[c.startIndex..<c.endIndex]
    let survivingIndices =
      Array(
        slice.startIndex.advancedBy(numericCast(test.numberToRemove)) ..<
        slice.endIndex
      )
    slice.removeFirst(test.numberToRemove)
    expectEqualSequence(
      test.expectedCollection,
      slice.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the tail of the slice",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.expectedCollection,
      survivingIndices.map { extractValue(slice[$0]).value },
      "removeFirst() shouldn't invalidate indices",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.collection,
      c.map { extractValue($0).value },
      "removeFirst() shouldn't mutate the collection that was sliced",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeFirst(n: Int)/slice/empty/semantics") {
  let c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeFirst(1) // Should trap.
}

self.test("\(testNamePrefix).removeFirst(n: Int)/slice/removeNegative/semantics") {
  let c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeFirst(-1) // Should trap.
}

self.test("\(testNamePrefix).removeFirst(n: Int)/slice/removeTooMany/semantics") {
  let c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeFirst(3) // Should trap.
}

//===----------------------------------------------------------------------===//

  } // addForwardCollectionTests

  public func addBidirectionalCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.Index : BidirectionalIndexType,
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    Collection.SubSequence.Index : BidirectionalIndexType,
    Collection.SubSequence.SubSequence == Collection.SubSequence,
    CollectionWithEquatableElement.Index : BidirectionalIndexType,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1,
    outOfBoundsSubscriptOffset: Int = 1
  ) {

    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(__FUNCTION__) {
      return
    }
    checksAdded.value.insert(__FUNCTION__)

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
      outOfBoundsIndexOffset: outOfBoundsIndexOffset,
      outOfBoundsSubscriptOffset: outOfBoundsSubscriptOffset)

    func makeWrappedCollection(elements: [OpaqueValue<Int>]) -> Collection {
      return makeCollection(elements.map(wrapValue))
    }

    testNamePrefix += String(Collection.Type)

//===----------------------------------------------------------------------===//
// last
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).last") {
  for test in subscriptRangeTests {
    let c = makeWrappedCollection(test.collection)
    let result = c.last
    if test.isEmpty {
      expectEmpty(result)
    } else {
      expectOptionalEqual(
        test.collection[test.count - 1],
        result.map(extractValue)
      ) { $0.value == $1.value }
    }
  }
}

//===----------------------------------------------------------------------===//
// removeLast()/slice
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeLast()/slice/semantics") {
  for test in removeLastTests.filter({ $0.numberToRemove == 1 }) {
    let c = makeWrappedCollection(test.collection)
    var slice = c[c.startIndex..<c.endIndex]
    let survivingIndices =
      Array(
        slice.startIndex ..<
        slice.endIndex.advancedBy(numericCast(-test.numberToRemove))
      )
    let removedElement = slice.removeLast()
    expectEqual(
      test.collection.last!.value,
      extractValue(removedElement).value)
    expectEqualSequence(
      test.expectedCollection,
      slice.map { extractValue($0).value },
      "removeLast() shouldn't mutate the head of the slice",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.expectedCollection,
      survivingIndices.map { extractValue(slice[$0]).value },
      "removeLast() shouldn't invalidate indices",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.collection.map { $0.value },
      c.map { extractValue($0).value },
      "removeLast() shouldn't mutate the collection that was sliced",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeLast()/slice/empty/semantics") {
  let c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  _ = slice.removeLast() // Should trap.
}

//===----------------------------------------------------------------------===//
// removeLast(n: Int)/slice
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).removeLast(n: Int)/slice/semantics") {
  for test in removeLastTests {
    let c = makeWrappedCollection(test.collection)
    var slice = c[c.startIndex..<c.endIndex]
    let survivingIndices =
      Array(
        slice.startIndex ..<
        slice.endIndex.advancedBy(numericCast(-test.numberToRemove))
      )
    slice.removeLast(test.numberToRemove)
    expectEqualSequence(
      test.expectedCollection,
      slice.map { extractValue($0).value },
      "removeLast() shouldn't mutate the head of the slice",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.expectedCollection,
      survivingIndices.map { extractValue(slice[$0]).value },
      "removeLast() shouldn't invalidate indices",
      stackTrace: SourceLocStack().with(test.loc)
    )
    expectEqualSequence(
      test.collection.map { $0.value },
      c.map { extractValue($0).value },
      "removeLast() shouldn't mutate the collection that was sliced",
      stackTrace: SourceLocStack().with(test.loc))
  }
}

self.test("\(testNamePrefix).removeLast(n: Int)/slice/empty/semantics") {
  let c = makeWrappedCollection(Array<OpaqueValue<Int>>())
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeLast(1) // Should trap.
}

self.test("\(testNamePrefix).removeLast(n: Int)/slice/removeNegative/semantics") {
  let c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeLast(-1) // Should trap.
}

self.test("\(testNamePrefix).removeLast(n: Int)/slice/removeTooMany/semantics") {
  let c = makeWrappedCollection([1010, 2020].map(OpaqueValue.init))
  var slice = c[c.startIndex..<c.startIndex]
  expectCrashLater()
  slice.removeLast(3) // Should trap.
}

//===----------------------------------------------------------------------===//
// Index
//===----------------------------------------------------------------------===//

if resiliencyChecks.creatingOutOfBoundsIndicesBehavior != .None {
  self.test("\(testNamePrefix).Index/OutOfBounds/Left/NonEmpty") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    let index = c.startIndex
    if resiliencyChecks.creatingOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      _blackHole(index.advancedBy(numericCast(-outOfBoundsIndexOffset)))
    } else {
      expectFailure {
        _blackHole(index.advancedBy(numericCast(-outOfBoundsIndexOffset)))
      }
    }
  }

  self.test("\(testNamePrefix).Index/OutOfBounds/Left/Empty") {
    let c = makeWrappedCollection([])
    let index = c.startIndex
    if resiliencyChecks.creatingOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      _blackHole(index.advancedBy(numericCast(-outOfBoundsIndexOffset)))
    } else {
      expectFailure {
        _blackHole(index.advancedBy(numericCast(-outOfBoundsIndexOffset)))
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// subscript(_: Index)
//===----------------------------------------------------------------------===//

if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior != .None {
  self.test("\(testNamePrefix).subscript(_: Index)/OutOfBounds/Left/NonEmpty/Get") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    var index = c.startIndex
    if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
      _blackHole(c[index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
        _blackHole(c[index])
      }
    }
  }

  self.test("\(testNamePrefix).subscript(_: Index)/OutOfBounds/Left/Empty/Get") {
    let c = makeWrappedCollection([])
    var index = c.startIndex
    if resiliencyChecks.subscriptOnOutOfBoundsIndicesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
      _blackHole(c[index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
        _blackHole(c[index])
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// subscript(_: Range)
//===----------------------------------------------------------------------===//

if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior != .None {
  self.test("\(testNamePrefix).subscript(_: Range)/OutOfBounds/Left/NonEmpty/Get") {
    let c = makeWrappedCollection([ 1010, 2020, 3030 ].map(OpaqueValue.init))
    var index = c.startIndex
    if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
      _blackHole(c[index..<index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
        _blackHole(c[index..<index])
      }
    }
  }

  self.test("\(testNamePrefix).subscript(_: Range)/OutOfBounds/Left/Empty/Get") {
    let c = makeWrappedCollection([])
    var index = c.startIndex
    if resiliencyChecks.subscriptRangeOnOutOfBoundsRangesBehavior == .Trap {
      expectCrashLater()
      index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
      _blackHole(c[index..<index])
    } else {
      expectFailure {
        index = index.advancedBy(numericCast(-outOfBoundsSubscriptOffset))
        _blackHole(c[index..<index])
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// dropLast()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).dropLast/semantics") {
  for test in dropLastTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.dropLast(test.dropElements)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// suffix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).suffix/semantics") {
  for test in suffixTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.suffix(test.maxLength)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//

  } // addBidirectionalCollectionTests

  public func addRandomAccessCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.Index : RandomAccessIndexType,
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    Collection.SubSequence.Index : RandomAccessIndexType,
    Collection.SubSequence.SubSequence == Collection.SubSequence,
    CollectionWithEquatableElement.Index : RandomAccessIndexType,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([]),
    resiliencyChecks: CollectionMisuseResiliencyChecks = .all,
    outOfBoundsIndexOffset: Int = 1,
    outOfBoundsSubscriptOffset: Int = 1
  ) {

    var testNamePrefix = testNamePrefix

    if checksAdded.value.contains(__FUNCTION__) {
      return
    }
    checksAdded.value.insert(__FUNCTION__)

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
      outOfBoundsIndexOffset: outOfBoundsIndexOffset,
      outOfBoundsSubscriptOffset: outOfBoundsSubscriptOffset)

    testNamePrefix += String(Collection.Type)

    func makeWrappedCollection(elements: [OpaqueValue<Int>]) -> Collection {
      return makeCollection(elements.map(wrapValue))
    }

//===----------------------------------------------------------------------===//
// prefix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).prefix/semantics") {
  for test in prefixTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.prefix(test.maxLength)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
// suffix()
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).suffix/semantics") {
  for test in suffixTests {
    let s = makeWrappedCollection(test.sequence.map(OpaqueValue.init))
    let result = s.suffix(test.maxLength)
    expectEqualSequence(test.expected, result.map(extractValue).map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

//===----------------------------------------------------------------------===//
  } // addRandomAccessCollectionTests
}

