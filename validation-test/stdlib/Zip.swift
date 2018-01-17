// -*- swift -*-
// RUN: %target-run-simple-swiftgyb
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest

var ZipTests = TestSuite("Zip")

// Check that the generic parameter is called 'Base'.
protocol TestProtocol1 {}

extension IteratorSequence where Base : TestProtocol1 {
  var _baseIsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}


// Check generic parameter names.
extension Zip2Sequence.Iterator
where Sequence1: TestProtocol1, Sequence2: TestProtocol1 {
  var _generator1IsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

// Check generic parameter names.
extension Zip2Sequence
where Sequence1 : TestProtocol1, Sequence2 : TestProtocol1 {
  var _sequence1IsTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

ZipTests.test("Sequence") {
  typealias Element = (OpaqueValue<Int>, OpaqueValue<Int32>)
  func compareElements(_ lhs: Element, rhs: Element) -> Bool {
    return lhs.0.value == rhs.0.value && lhs.1.value == rhs.1.value
  }

  for test in zipTests {
    let s = MinimalSequence<OpaqueValue<Int>>(
      elements: test.sequence.map(OpaqueValue.init))
    let other = MinimalSequence<OpaqueValue<Int32>>(
      elements: test.other.map(OpaqueValue.init))
    var result = zip(s, other)
    expectType(
      Zip2Sequence<MinimalSequence<OpaqueValue<Int>>, MinimalSequence<OpaqueValue<Int32>>>.self,
      &result)

    // Check for expected result and check the Zip2Sequence's Sequence
    // conformance.
    checkSequence(
      test.expected.map { (OpaqueValue($0), OpaqueValue($1)) }, result,
      stackTrace: SourceLocStack().with(test.loc), sameValue: compareElements)

    // Check leftovers *after* doing checkSequence(), not before, to ensure
    // that checkSequence() didn't force us to consume more elements than
    // needed.
    expectEqual(
      test.expectedLeftoverSequence, s.map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
    expectEqual(
      test.expectedLeftoverOther, other.map { $0.value },
      stackTrace: SourceLocStack().with(test.loc))
  }
}

ZipTests.test("Collections") {
  typealias Element = (OpaqueValue<Int>, OpaqueValue<Int32>)
  typealias SequenceType = Zip2Sequence<
    MinimalCollection<OpaqueValue<Int>>,MinimalCollection<OpaqueValue<Int32>>>
  typealias CollectionType = Zip2Collection<
    MinimalCollection<OpaqueValue<Int>>,MinimalCollection<OpaqueValue<Int32>>>

  func compareElements(_ lhs: Element, rhs: Element) -> Bool {
    return lhs.0.value == rhs.0.value && lhs.1.value == rhs.1.value
  }

  expectCollectionAssociatedTypes(
    collectionType: CollectionType.self,
    iteratorType: SequenceType.Iterator.self,
    subSequenceType: Slice<CollectionType>.self,
    indexType: CollectionType.Index.self,
    indicesType: DefaultIndices<CollectionType>.self)
    
  for test in zipTests {
    let left = MinimalCollection<OpaqueValue<Int>>(
      elements: test.sequence.map(OpaqueValue.init))
    let right = MinimalCollection<OpaqueValue<Int32>>(
      elements: test.other.map(OpaqueValue.init))

    var result = zip(left, right)
    expectType(CollectionType.self, &result)

    checkCollection(
      test.expected.map { (OpaqueValue($0), OpaqueValue($1)) }, 
      result, sameValue: compareElements)      
  }
}

ZipTests.test("RandomAccessCollections") {
  typealias Element = (OpaqueValue<Int>, OpaqueValue<Int32>)
  typealias SequenceType = Zip2Sequence<
    MinimalRandomAccessCollection<OpaqueValue<Int>>,
    MinimalRandomAccessCollection<OpaqueValue<Int32>>>
  typealias CollectionType = Zip2Collection<
    MinimalRandomAccessCollection<OpaqueValue<Int>>,
    MinimalRandomAccessCollection<OpaqueValue<Int32>>>

  func compareElements(_ lhs: Element, rhs: Element) -> Bool {
    return lhs.0.value == rhs.0.value && lhs.1.value == rhs.1.value
  }
      
  expectCollectionAssociatedTypes(
    collectionType: CollectionType.self,
    iteratorType: SequenceType.Iterator.self,
    subSequenceType: Slice<CollectionType>.self,
    indexType: CollectionType.Index.self,
    indicesType: DefaultIndices<CollectionType>.self)

  for test in zipTests {
    let left = MinimalRandomAccessCollection<OpaqueValue<Int>>(
      elements: test.sequence.map(OpaqueValue.init))
    let right = MinimalRandomAccessCollection<OpaqueValue<Int32>>(
      elements: test.other.map(OpaqueValue.init))

    var result = zip(left, right)
    expectType(CollectionType.self, &result)
    checkRandomAccessCollection(
      test.expected.map { (OpaqueValue($0), OpaqueValue($1)) }, 
      result, sameValue: compareElements)
  }
}

ZipTests.test("Collection.isEmpty") {
  expectTrue(zip(0..<0,0..<0).isEmpty)
  expectTrue(zip(0..<1,0..<0).isEmpty)
  expectTrue(zip(0..<0,0..<1).isEmpty)
  expectFalse(zip(0..<1,0..<1).isEmpty)
  expectFalse(zip(0..<2,0..<3).isEmpty)
  expectFalse(zip(0..<3,0..<2).isEmpty)
}

ZipTests.test("Collection.count") {  
  expectEqual(zip(0..<0,0..<0).underestimatedCount, 0)
  expectEqual(zip(0..<1,0..<0).underestimatedCount, 0)
  expectEqual(zip(0..<0,0..<1).underestimatedCount, 0)
  expectEqual(zip(0..<2,0..<1).underestimatedCount, 1)
  expectEqual(zip(0..<1,0..<2).underestimatedCount, 1)
  expectEqual(zip(0..<2,0..<2).underestimatedCount, 2)
  expectEqual(zip(0..<0,0..<0).count, 0)
  expectEqual(zip(0..<1,0..<0).count, 0)
  expectEqual(zip(0..<0,0..<1).count, 0)
  expectEqual(zip(0..<2,0..<1).count, 1)
  expectEqual(zip(0..<1,0..<2).count, 1)
  expectEqual(zip(0..<2,0..<2).count, 2)
  
  // underestimatedCount is the minimum of the two bases, to 
  expectEqual(zip(0..<2,(0..<2).lazy.filter { _ in true }).underestimatedCount, 0)
  expectEqual(zip((0..<2).lazy.filter { _ in true }, 0..<2).underestimatedCount, 0)
  expectEqual(zip((0..<2).lazy.filter { _ in true }, (0..<2).lazy.map { $0 }).underestimatedCount, 0)
}

ZipTests.test("Equatable") {
  checkEquatable([
    zip("",0..<99),
    zip("abcdef",0..<0),
    zip("xyz",0..<3)
  ], oracle: { $0 == $1 })
}

ZipTests.test("Sequence+Collection") {
  typealias Element = (OpaqueValue<Int>, OpaqueValue<Int32>)
  func compareElements(_ lhs: Element, rhs: Element) -> Bool {
    return lhs.0.value == rhs.0.value && lhs.1.value == rhs.1.value
  }

  for test in zipTests {
    let s = MinimalSequence<OpaqueValue<Int>>(
      elements: test.sequence.map(OpaqueValue.init))
    let other = MinimalCollection<OpaqueValue<Int32>>(
      elements: test.other.map(OpaqueValue.init))
    var result = zip(s, other)
    expectType(
      Zip2Sequence<
        MinimalSequence<OpaqueValue<Int>>, 
        MinimalCollection<OpaqueValue<Int32>>>.self,
      &result)

    // Check for expected result and check the Zip2Sequence's Sequence
    // conformance.
    checkSequence(
      test.expected.map { (OpaqueValue($0), OpaqueValue($1)) }, result,
      stackTrace: SourceLocStack().with(test.loc), sameValue: compareElements)
  }
}

runAllTests()
