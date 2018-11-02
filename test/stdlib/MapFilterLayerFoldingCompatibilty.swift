// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out3 -swift-version 3 && %target-run %t/a.out3
// RUN: %target-build-swift %s -o %t/a.out4 -swift-version 4 && %target-run %t/a.out4
// RUN: %target-build-swift %s -o %t/a.out5 -swift-version 5 && %target-run %t/a.out5
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
