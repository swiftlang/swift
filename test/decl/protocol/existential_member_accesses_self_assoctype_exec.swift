// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Temporarily disabled due to a runtime crash.
// REQUIRES: rdar83066987

import StdlibUnittest

let Tests = TestSuite(#file)

protocol P {
  associatedtype Assoc = Self

  func getString() -> String

  func covariantSelfSimple() -> Self
  func covariantSelfArray() -> Array<Self>
  func covariantSelfDictionary() -> [String : Self]
  func covariantSelfClosure(_: (Self) -> Void)

  var covariantSelfPropSimple: Self { get }
  var covariantSelfPropArray: Array<Self> { get }
  var covariantSelfPropDictionary: [String : Self] { get }
  var covariantSelfPropClosure: ((Self) -> Void) -> Void { get }

  subscript(covariantSelfSubscriptSimple _: Void) -> Self { get }
  subscript(covariantSelfSubscriptArray _: Void) -> Array<Self> { get }
  subscript(covariantSelfSubscriptDictionary _: Void) -> [String : Self] { get }
  subscript(covariantSelfSubscriptClosure _: (Self) -> Void) -> Void { get }
}
extension P {
  func covariantSelfSimple() -> Self { self }
  func covariantSelfArray() -> Array<Self> { [self] }
  func covariantSelfDictionary() -> [String : Self] { [#file : self] }
  func covariantSelfClosure(_ arg: (Self) -> Void) { arg(self) }

  var covariantSelfPropSimple: Self { self }
  var covariantSelfPropArray: Array<Self> { [self] }
  var covariantSelfPropDictionary: [String : Self] { [#file : self] }
  var covariantSelfPropClosure: ((Self) -> Void) -> Void { { $0(self) } }

  subscript(covariantSelfSubscriptSimple _: Void) -> Self { self }
  subscript(covariantSelfSubscriptArray _: Void) -> Array<Self> { [self] }
  subscript(covariantSelfSubscriptDictionary _: Void) -> [String : Self] { [#file : self] }
  subscript(covariantSelfSubscriptClosure arg: (Self) -> Void) -> Void { arg(self) }
}

Tests.test("Basic") {
  let collection: Collection = [0, 0, 0]

  expectEqual(3, collection.count)
}

Tests.test("Covariant 'Self' erasure") {
  struct S: P {
    static let str = "Success"
    func getString() -> String { Self.str }
  }

  let p: P = S()

  // Partial Application
  do {
    let covariantSelfSimplePartialApp = p.covariantSelfSimple
    let covariantSelfArrayPartialApp = p.covariantSelfArray
    let covariantSelfDictionaryPartialApp = p.covariantSelfDictionary
    let covariantSelfClosurePartialApp = p.covariantSelfClosure

    expectEqual(S.str, covariantSelfSimplePartialApp().getString())
    expectEqual(S.str, covariantSelfArrayPartialApp().first.unsafelyUnwrapped.getString())
    expectEqual(S.str, covariantSelfDictionaryPartialApp()[#file].unsafelyUnwrapped.getString())
    covariantSelfClosurePartialApp { expectEqual(S.str, $0.getString()) }
  }

  // Instance method reference on metatype
  do {
    let covariantSelfSimpleRef = P.covariantSelfSimple
    let covariantSelfArrayRef = P.covariantSelfArray
    let covariantSelfDictionaryRef = P.covariantSelfDictionary
    let covariantSelfClosureRef = P.covariantSelfClosure

    expectEqual(S.str, covariantSelfSimpleRef(p)().getString())
    expectEqual(S.str, covariantSelfArrayRef(p)().first.unsafelyUnwrapped.getString())
    expectEqual(S.str, covariantSelfDictionaryRef(p)()[#file].unsafelyUnwrapped.getString())
    covariantSelfClosureRef(p)({ expectEqual(S.str, $0.getString()) })
  }

  // Regular calls
  expectEqual(S.str, p.covariantSelfSimple().getString())
  expectEqual(S.str, p.covariantSelfArray().first.unsafelyUnwrapped.getString())
  expectEqual(S.str, p.covariantSelfDictionary()[#file].unsafelyUnwrapped.getString())
  p.covariantSelfClosure { expectEqual(S.str, $0.getString()) }

  expectEqual(S.str, p.covariantSelfPropSimple.getString())
  expectEqual(S.str, p.covariantSelfPropArray.first.unsafelyUnwrapped.getString())
  expectEqual(S.str, p.covariantSelfPropDictionary[#file].unsafelyUnwrapped.getString())
  p.covariantSelfPropClosure { expectEqual(S.str, $0.getString()) }

  expectEqual(S.str, p[covariantSelfSubscriptSimple: ()].getString())
  expectEqual(S.str, p[covariantSelfSubscriptArray: ()].first.unsafelyUnwrapped.getString())
  expectEqual(S.str, p[covariantSelfSubscriptDictionary: ()][#file].unsafelyUnwrapped.getString())
  p[covariantSelfSubscriptClosure: { expectEqual(S.str, $0.getString()) }]

  expectEqual(S.str, (S() as P).getString())

  expectEqual(true, p is P)
  expectEqual(true, S() is P)
}

protocol MyIntCollection:
  BidirectionalCollection where Element == Int,
                                SubSequence == ArraySlice<Element>,
                                Index == Int {}
extension Array: MyIntCollection where Element == Int {}

Tests.test("Known associated types") {
  let erasedIntArr: MyIntCollection = [5, 8, 1, 9, 3, 8]

  expectEqual(6, erasedIntArr.count)
  expectEqual(5, erasedIntArr[Int.zero])
  expectEqual(8, erasedIntArr.last.unsafelyUnwrapped)
  expectEqual([5, 8, 1, 9, 3], erasedIntArr.dropLast())
  expectEqual([8, 3, 9, 1, 8, 5], erasedIntArr.reversed())
  expectEqual(9, erasedIntArr.max().unsafelyUnwrapped)
  expectEqual([1, 3, 5, 8, 8, 9], erasedIntArr.sorted())
  expectEqual(false, erasedIntArr.contains(Int.zero))
  expectEqual([5, 8],
    erasedIntArr[
      erasedIntArr.startIndex...erasedIntArr.firstIndex(
        of: erasedIntArr.last.unsafelyUnwrapped
      ).unsafelyUnwrapped
    ]
  )
}

runAllTests()
