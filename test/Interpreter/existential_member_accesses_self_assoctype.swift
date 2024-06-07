// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let Tests = TestSuite(#file)

protocol CovariantSelf {
  var id: Int { get }

  func result() -> Self
  func resultArray() -> Array<Self>
  func resultDictionary() -> [String : Self]
  func param(_: (Self) -> Void)
  func paramVariadic(_: (Self...) -> Void)

  var propResult: Self { get }
  var propResultArray: Array<Self> { get }
  var propResultDictionary: [String : Self] { get }
  var propParam: ((Self) -> Void) -> Void { get }
  var propParamVariadic: ((Self...) -> Void) -> Void { get }

  subscript(result _: Void) -> Self { get }
  subscript(resultArray _: Void) -> Array<Self> { get }
  subscript(resultDictionary _: Void) -> [String : Self] { get }
  subscript(param _: (Self) -> Void) -> Void { get }
  subscript(paramVariadic _: (Self...) -> Void) -> Void { get }
}
struct CovariantSelfImpl: CovariantSelf {
  let id: Int

  func result() -> Self { self }
  func resultArray() -> Array<Self> { [self] }
  func resultDictionary() -> [String : Self] { [#file : self] }
  func param(_ arg: (Self) -> Void) { arg(self) }
  func paramVariadic(_ arg: (Self...) -> Void) { arg(self) }

  var propResult: Self { self }
  var propResultArray: Array<Self> { [self] }
  var propResultDictionary: [String : Self] { [#file : self] }
  var propParam: ((Self) -> Void) -> Void { { $0(self) } }
  var propParamVariadic: ((Self...) -> Void) -> Void { { $0(self) } }

  subscript(result _: Void) -> Self { self }
  subscript(resultArray _: Void) -> Array<Self> { [self] }
  subscript(resultDictionary _: Void) -> [String : Self] { [#file : self] }
  subscript(param arg: (Self) -> Void) -> Void { arg(self) }
  subscript(paramVariadic arg: (Self...) -> Void) -> Void { arg(self) }
}
Tests.test("Covariant Self erasure") {
  let id = Int.random(in: Int.min...Int.max)
  let p: any CovariantSelf = CovariantSelfImpl(id: id)

  expectTrue(p is CovariantSelfImpl)

  // Partial applications
  do {
    let result = p.result
    let resultArray = p.resultArray
    let resultDictionary = p.resultDictionary
    let param = p.param
    let paramVariadic = p.paramVariadic

    expectEqual(id, result().id)
    expectEqual(id, resultArray().first.unsafelyUnwrapped.id)
    expectEqual(id, resultDictionary()[#file].unsafelyUnwrapped.id)
    param {
      expectEqual(id, $0.id)
    }
    paramVariadic {
      expectEqual(id, $0.first.unsafelyUnwrapped.id)
    }
  }

  // Unbound method references
  do {
    let result = (any CovariantSelf).result
    let resultArray = (any CovariantSelf).resultArray
    let resultDictionary = (any CovariantSelf).resultDictionary
    let param = (any CovariantSelf).param
    let paramVariadic = (any CovariantSelf).paramVariadic

    expectEqual(id, result(p)().id)
    expectEqual(id, resultArray(p)().first.unsafelyUnwrapped.id)
    expectEqual(id, resultDictionary(p)()[#file].unsafelyUnwrapped.id)
    param(p)(
      { expectEqual(id, $0.id) }
    )
    paramVariadic(p)(
      { expectEqual(id, $0.first.unsafelyUnwrapped.id) }
    )
  }

  // Instance member calls
  expectEqual(id, p.id)

  expectEqual(id, p.result().id)
  expectEqual(id, p.resultArray().first.unsafelyUnwrapped.id)
  expectEqual(id, p.resultDictionary()[#file].unsafelyUnwrapped.id)
  p.param {
    expectEqual(id, $0.id)
  }
  p.paramVariadic {
    expectEqual(id, $0.first.unsafelyUnwrapped.id)
  }

  expectEqual(id, p.propResult.id)
  expectEqual(id, p.propResultArray.first.unsafelyUnwrapped.id)
  expectEqual(id, p.propResultDictionary[#file].unsafelyUnwrapped.id)
  p.propParam {
    expectEqual(id, $0.id)
  }
  p.propParamVariadic {
    expectEqual(id, $0.first.unsafelyUnwrapped.id)
  }

  expectEqual(id, p[result: ()].id)
  expectEqual(id, p[resultArray: ()].first.unsafelyUnwrapped.id)
  expectEqual(id, p[resultDictionary: ()][#file].unsafelyUnwrapped.id)
  p[
    param: { expectEqual(id, $0.id) }
  ]
  p[
    paramVariadic: { expectEqual(id, $0.first.unsafelyUnwrapped.id) }
  ]
}

protocol CovariantAssoc {
  associatedtype Assoc: CovariantAssoc

  var id: Int { get }

  func result() -> Assoc
  func resultArray() -> Array<Assoc>
  func resultDictionary() -> [String : Assoc]
  func param(_: (Assoc) -> Void)
  func paramVariadic(_: (Assoc...) -> Void)

  var propResult: Assoc { get }
  var propResultArray: Array<Assoc> { get }
  var propResultDictionary: [String : Assoc] { get }
  var propParam: ((Assoc) -> Void) -> Void { get }
  var propParamVariadic: ((Assoc...) -> Void) -> Void { get }

  subscript(result _: Void) -> Assoc { get }
  subscript(resultArray _: Void) -> Array<Assoc> { get }
  subscript(resultDictionary _: Void) -> [String : Assoc] { get }
  subscript(param _: (Assoc) -> Void) -> Void { get }
  subscript(paramVariadic _: (Assoc...) -> Void) -> Void { get }
}
struct CovariantAssocImpl: CovariantAssoc {
  typealias Assoc = Self

  let id: Int

  func result() -> Assoc { self }
  func resultArray() -> Array<Assoc> { [self] }
  func resultDictionary() -> [String : Assoc] { [#file : self] }
  func param(_ arg: (Assoc) -> Void) { arg(self) }
  func paramVariadic(_ arg: (Assoc...) -> Void) { arg(self) }

  var propResult: Assoc { self }
  var propResultArray: Array<Assoc> { [self] }
  var propResultDictionary: [String : Assoc] { [#file : self] }
  var propParam: ((Assoc) -> Void) -> Void { { $0(self) } }
  var propParamVariadic: ((Assoc...) -> Void) -> Void { { $0(self) } }

  subscript(result _: Void) -> Assoc { self }
  subscript(resultArray _: Void) -> Array<Assoc> { [self] }
  subscript(resultDictionary _: Void) -> [String : Assoc] { [#file : self] }
  subscript(param arg: (Assoc) -> Void) -> Void { arg(self) }
  subscript(paramVariadic arg: (Assoc...) -> Void) -> Void { arg(self) }
}
Tests.test("Covariant associated type erasure") {
  let id = Int.random(in: Int.min...Int.max)
  let p: any CovariantAssoc = CovariantAssocImpl(id: id)

  expectTrue(p is CovariantAssocImpl)
  expectTrue(p.result() is CovariantAssocImpl)

  // Partial applications
  do {
    let result = p.result
    let resultArray = p.resultArray
    let resultDictionary = p.resultDictionary
    let param = p.param
    let paramVariadic = p.paramVariadic

    expectEqual(id, result().id)
    expectEqual(id, resultArray().first.unsafelyUnwrapped.id)
    expectEqual(id, resultDictionary()[#file].unsafelyUnwrapped.id)
    param {
      expectEqual(id, $0.id)
    }
    paramVariadic {
      expectEqual(id, $0.first.unsafelyUnwrapped.id)
    }
  }

  // Unbound method references
  do {
    let result = (any CovariantAssoc).result
    let resultArray = (any CovariantAssoc).resultArray
    let resultDictionary = (any CovariantAssoc).resultDictionary
    let param = (any CovariantAssoc).param
    let paramVariadic = (any CovariantAssoc).paramVariadic

    expectEqual(id, result(p)().id)
    expectEqual(id, resultArray(p)().first.unsafelyUnwrapped.id)
    expectEqual(id, resultDictionary(p)()[#file].unsafelyUnwrapped.id)
    param(p)(
      { expectEqual(id, $0.id) }
    )
    paramVariadic(p)(
      { expectEqual(id, $0.first.unsafelyUnwrapped.id) }
    )
  }

  // Instance member calls
  expectEqual(id, p.id)

  expectEqual(id, p.result().id)
  expectEqual(id, p.resultArray().first.unsafelyUnwrapped.id)
  expectEqual(id, p.resultDictionary()[#file].unsafelyUnwrapped.id)
  p.param {
    expectEqual(id, $0.id)
  }
  p.paramVariadic {
    expectEqual(id, $0.first.unsafelyUnwrapped.id)
  }

  expectEqual(id, p.propResult.id)
  expectEqual(id, p.propResultArray.first.unsafelyUnwrapped.id)
  expectEqual(id, p.propResultDictionary[#file].unsafelyUnwrapped.id)
  p.propParam {
    expectEqual(id, $0.id)
  }
  p.propParamVariadic {
    expectEqual(id, $0.first.unsafelyUnwrapped.id)
  }

  expectEqual(id, p[result: ()].id)
  expectEqual(id, p[resultArray: ()].first.unsafelyUnwrapped.id)
  expectEqual(id, p[resultDictionary: ()][#file].unsafelyUnwrapped.id)
  p[
    param: { expectEqual(id, $0.id) }
  ]
  p[
    paramVariadic: { expectEqual(id, $0.first.unsafelyUnwrapped.id) }
  ]
}

func assertExactType<T>(of _: T, is _: T.Type) {}

protocol CollectionOfBinaryInteger: Collection where Element: BinaryInteger {}

extension Array: CollectionOfBinaryInteger where Element: BinaryInteger {}

Tests.test("Collection of BinaryInteger") {
  let c: any CollectionOfBinaryInteger = [1, 2, 3, 4]

  expectEqual(4, c.count)
  expectFalse(c.isEmpty)
  expectTrue(c.startIndex is Int)
  expectTrue(c.first.unsafelyUnwrapped is Int)
  expectEqual(
    [0, 1, 0, 2],
    c.map { (elt: BinaryInteger) in elt.trailingZeroBitCount }
  )
}

protocol RandomAccessCollectionOfInt: RandomAccessCollection
where Element == Int, Index == Int, SubSequence == Array<Element>.SubSequence {}

extension Array: RandomAccessCollectionOfInt where Element == Int {}

Tests.test("RandomAccessCollection of Int") {
  let c: any RandomAccessCollectionOfInt = [5, 8, 1, 9, 3, 8]

  expectEqual(6, c.count)
  expectEqual(0, c.startIndex)
  expectEqual(6, c.endIndex)
  expectEqual(1, c.index(after: c.startIndex))
  expectEqual(5, c.index(before: c.endIndex))
  expectEqual(2, c.index(c.startIndex, offsetBy: 2))
  expectEqual(6, c.distance(from: c.startIndex, to: c.endIndex))
  expectEqual(5, c[0])
  expectEqual(5, c.first.unsafelyUnwrapped)
  expectEqual(9, c.first(where: { $0 > 8 }).unsafelyUnwrapped)
  expectEqual(8, c.last.unsafelyUnwrapped)
  expectEqual([5, 8, 1, 9, 3], c.dropLast())
  expectEqual([8, 3, 9, 1, 8, 5], c.reversed())
  expectEqual(9, c.max().unsafelyUnwrapped)
  expectEqual([1, 3, 5, 8, 8, 9], c.sorted())
  expectEqual([9, 8, 8, 5, 3, 1], c.sorted(by: >))
  expectEqual([8, 9, 8], c.filter { $0 > 7 })
  expectEqual([4, 7, 0, 8, 2, 7], c.map { $0 - 1 })
  expectEqual(34, c.reduce(0, +))
  expectEqual(5, c.lastIndex(of: 8).unsafelyUnwrapped)
  expectEqual([5], c[c.startIndex..<c.firstIndex(of: 8).unsafelyUnwrapped])
  expectEqual([[5, 8], [9, 3, 8]], c.split(separator: 1))
  expectNotNil(
    c.withContiguousStorageIfAvailable { buffer in
      expectEqual(5, buffer[0])
    }
  )
  expectFalse(c.contains(0))
  expectFalse(c.contains { $0 > 10 })
  expectFalse(c.allSatisfy { $0 > 1 })

  do {
    var index = c.startIndex
    c.formIndex(after: &index)
    expectEqual(1, index)
    c.formIndex(before: &index)
    expectEqual(0, index)
  }

  do {
    let slice = c.dropFirst(3)
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([9, 3, 8], slice)
  }
  do {
    let slice = c.dropLast(3)
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([5, 8, 1], slice)
  }
  do {
    let slice = c.drop { $0 < 9 }
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([9, 3, 8], slice)
  }
  do {
    let slice = c.suffix(3)
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([9, 3, 8], slice)
  }
  do {
    let slice = c.prefix(3)
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([5, 8, 1], slice)
  }
  do {
    let slice = c.prefix { $0 < 9 }
    assertExactType(of: slice, is: ArraySlice<Int>.self)
    expectEqual([5, 8, 1], slice)
  }
}

protocol RangeReplaceableMutableIntCollection:
  RangeReplaceableCollection, MutableCollection, BidirectionalCollection
where Element == Int, Index == Int, SubSequence == Array<Element>.SubSequence {}

extension Array: RangeReplaceableMutableIntCollection where Element == Int {}

Tests.test("RangeReplaceableCollection & MutableCollection of Int") {
  var c: any RangeReplaceableMutableIntCollection = [5, 8, 1]

  c.append(2)
  expectEqual([5, 8, 1, 2], c[...])
  expectEqual(2, c.popLast().unsafelyUnwrapped)
  c.swapAt(c.startIndex, c.index(before: c.endIndex))
  expectEqual([1, 8, 5], c[...])
  c.removeFirst()
  c.removeLast()
  expectEqual([8], c[...])
  c.append(contentsOf: 4...7)
  expectEqual([8, 4, 5, 6, 7], c[...])
  expectEqual(4, c.remove(at: 1))
  c.replaceSubrange(2..<4, with: 1...3)
  expectEqual([8, 5, 1, 2, 3], c[...])
  c.removeSubrange(0..<2)
  expectEqual([1, 2, 3], c[...])
}

runAllTests()
