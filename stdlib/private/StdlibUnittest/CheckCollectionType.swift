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
      start: advance(i, numericCast(bounds.startIndex)),
      end: advance(i, numericCast(bounds.endIndex)))
  }

  public init(
    expected: [Int], collection: [Int], bounds: Range<Int>,
    count: Int,
    file: String = __FILE__, line: UWord = __LINE__
  ) {
    self.expected = expected.map { OpaqueValue($0) }
    self.collection = collection.map { OpaqueValue($0) }
    self.bounds = bounds
    self.count = count
    self.loc = SourceLoc(file, line, comment: "test data")
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

extension TestSuite {
  public func addForwardCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    var testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([])
  ) {

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
      checksAdded: checksAdded)

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
// subscript(_: Index)
//===----------------------------------------------------------------------===//

self.test("\(testNamePrefix).subscript(_: Index)/semantics") {
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
      numericCast(distance(c.startIndex, $0)) as Int
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
      numericCast(distance(c.startIndex, $0)) as Int
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

  } // addForwardCollectionTests

  public func addBidirectionalCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.Index : BidirectionalIndexType,
    CollectionWithEquatableElement.Index : BidirectionalIndexType,
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    var testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([])
  ) {
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
      checksAdded: checksAdded)

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

  } // addBidirectionalCollectionTests

  public func addRandomAccessCollectionTests<
    Collection : CollectionType,
    CollectionWithEquatableElement : CollectionType
    where
    Collection.Index : RandomAccessIndexType,
    CollectionWithEquatableElement.Index : RandomAccessIndexType,
    Collection.SubSequence : CollectionType,
    Collection.SubSequence.Generator.Element == Collection.Generator.Element,
    CollectionWithEquatableElement.Generator.Element : Equatable
  >(
    var testNamePrefix: String = "",
    makeCollection: ([Collection.Generator.Element]) -> Collection,
    wrapValue: (OpaqueValue<Int>) -> Collection.Generator.Element,
    extractValue: (Collection.Generator.Element) -> OpaqueValue<Int>,

    makeCollectionOfEquatable: ([CollectionWithEquatableElement.Generator.Element]) -> CollectionWithEquatableElement,
    wrapValueIntoEquatable: (MinimalEquatableValue) -> CollectionWithEquatableElement.Generator.Element,
    extractValueFromEquatable: ((CollectionWithEquatableElement.Generator.Element) -> MinimalEquatableValue),

    checksAdded: Box<Set<String>> = Box([])
  ) {

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
      checksAdded: checksAdded)

    testNamePrefix += String(Collection.Type)
  } // addRandomAccessCollectionTests
}

