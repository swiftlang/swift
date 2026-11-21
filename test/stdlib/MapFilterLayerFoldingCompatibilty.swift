// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-codesign %t/a.out4 && %target-run %t/a.out4
// RUN: %target-build-swift %s -o %t/a.out5 -swift-version 5 && %target-codesign %t/a.out5 && %target-run %t/a.out5
// REQUIRES: executable_test

import StdlibUnittest

#if swift(>=5)
let swiftVersion = ">=5"
#else
let swiftVersion = "<5"
#endif

let tests = TestSuite("MapFilterLayerFoldingCompatibility")

tests.test("Double filter type/Sequence/\(swiftVersion)") {
  func foldingLevels<S : Sequence>(_ xs: S) {
    var result = xs.lazy.filter { _ in true }.filter { _ in true }
#if swift(>=5)
    expectType(LazyFilterSequence<S>.self, &result)
#else
    expectType(LazyFilterSequence<LazyFilterSequence<S>>.self, &result)
#endif
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<S : Sequence>(_ xs: S) {
    typealias ExpectedType = LazyFilterSequence<LazyFilterSequence<S>>
    var result: ExpectedType = xs.lazy
      .filter { _ in true }.filter { _ in true }
    expectType(ExpectedType.self, &result)
  }
  backwardCompatible(Array(0..<10))
}

tests.test("Double filter type/Collection/\(swiftVersion)") {
  func foldingLevels<C : Collection>(_ xs: C) {
    var result = xs.lazy.filter { _ in true }.filter { _ in true }
#if swift(>=5)
    expectType(LazyFilterCollection<C>.self, &result)
#else
    expectType(LazyFilterCollection<LazyFilterCollection<C>>.self, &result)
#endif
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<C : Collection>(_ xs: C) {
    typealias ExpectedType = LazyFilterCollection<LazyFilterCollection<C>>
    var result: ExpectedType = xs.lazy
      .filter { _ in true }.filter { _ in true }
    expectType(ExpectedType.self, &result)
  }
  backwardCompatible(Array(0..<10))
}

tests.test("Double filter predicate order/\(swiftVersion)") {
  var calls: [Int] = []
  let result = AnySequence(0..<4).lazy
    .filter { calls.append(10 + $0); return $0 % 2 == 0 }
    .filter { calls.append(20 + $0); return $0 == 0 }
  expectEqual([], calls)
  expectEqual([0], Array(result))
  // The first predicate runs first, and the second one only runs for the
  // elements that the first one accepts.
  expectEqual([10, 20, 11, 12, 22, 13], calls)
}

tests.test("Double map type/Sequence/\(swiftVersion)") {
  func foldingLevels<S : Sequence>(_ xs: S) {
    var result = xs.lazy.map { $0 }.map { $0 }
#if swift(>=5)
    expectType(LazyMapSequence<S, S.Element>.self, &result)
#else
    expectType(
      LazyMapSequence<LazyMapSequence<S, S.Element>, S.Element>.self,
      &result)
#endif
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<S : Sequence>(_ xs: S) {
    typealias ExpectedType =
      LazyMapSequence<LazyMapSequence<S, S.Element>, S.Element>
    var result: ExpectedType = xs.lazy.map { $0 }.map { $0 }
    expectType(ExpectedType.self, &result)
  }
  backwardCompatible(Array(0..<10))
}

tests.test("Double map type/Collection/\(swiftVersion)") {
  func foldingLevels<C : Collection>(_ xs: C) {
    var result = xs.lazy.map { $0 }.map { $0 }
#if swift(>=5)
    expectType(LazyMapCollection<C, C.Element>.self, &result)
#else
    expectType(
      LazyMapCollection<LazyMapCollection<C, C.Element>, C.Element>.self,
      &result)
#endif
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<C : Collection>(_ xs: C) {
    typealias ExpectedType =
      LazyMapCollection<LazyMapCollection<C, C.Element>, C.Element>
    var result: ExpectedType = xs.lazy.map { $0 }.map { $0 }
    expectType(ExpectedType.self, &result)
  }
  backwardCompatible(Array(0..<10))
}

runAllTests()
