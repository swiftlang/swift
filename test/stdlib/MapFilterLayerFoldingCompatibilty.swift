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

tests.test("Combined CompactMap Type/Sequence") {
  func foldingLevels<S: Sequence>(_ xs: S) {
    var result0 = xs.lazy.filter({ _ in true }).map({ $0 })
    var result1 = xs.lazy.map({ $0 }).filter({ _ in true })
    var result2 = xs.lazy.compactMap({ $0 })
      .filter({ _ in true }).map({ $0 })
    var result3 = xs.lazy.map({ $0 }).filter({ _ in true })
      .compactMap({ $0 })
#if swift(>=5)
    typealias ExpectedType0 = LazyCompactMapSequence<S, S.Element>
    typealias ExpectedType1 = ExpectedType0
    typealias ExpectedType2 = ExpectedType0
    typealias ExpectedType3 = ExpectedType0
#else
    typealias ExpectedType0 = LazyMapSequence<LazyFilterSequence<S>, S.Element>
    typealias ExpectedType1 = LazyFilterSequence<LazyMapSequence<S, S.Element>>
    typealias ExpectedType2 = LazyMapSequence<LazyFilterSequence<LazyMapSequence<LazyFilterSequence<LazyMapSequence<S, S.Element?>>, S.Element>>, S.Element>
    typealias ExpectedType3 = LazyMapSequence<LazyFilterSequence<LazyMapSequence<LazyFilterSequence<LazyMapSequence<S, S.Element>>, S.Element?>>, S.Element>
#endif
    expectType(ExpectedType0.self, &result0)
    expectType(ExpectedType1.self, &result1)
    expectType(ExpectedType2.self, &result2)
    expectType(ExpectedType3.self, &result3)
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<S: Sequence>(_ xs: S) {
    typealias ExpectedType0 = LazyMapSequence<LazyFilterSequence<S>, S.Element>
    typealias ExpectedType1 = LazyFilterSequence<LazyMapSequence<S, S.Element>>
    var result0: ExpectedType0 = xs.lazy.filter({ _ in true }).map({ $0 })
    var result1: ExpectedType1 = xs.lazy.map({ $0 }).filter({ _ in true })
    expectType(ExpectedType0.self, &result0)
    expectType(ExpectedType1.self, &result1)
  }
  backwardCompatible(Array(0..<10))
}

tests.test("Combined CompactMap Type/Collection") {
  func foldingLevels<C: Collection>(_ xs: C) {
    var result0 = xs.lazy.filter({ _ in true }).map({ $0 })
    var result1 = xs.lazy.map({ $0 }).filter({ _ in true })
    var result2 = xs.lazy.compactMap({ $0 })
      .filter({ _ in true }).map({ $0 })
    var result3 = xs.lazy.map({ $0 }).filter({ _ in true })
      .compactMap({ $0 })
#if swift(>=5)
    typealias ExpectedType0 = LazyCompactMapCollection<C, C.Element>
    typealias ExpectedType1 = ExpectedType0
    typealias ExpectedType2 = ExpectedType0
    typealias ExpectedType3 = ExpectedType0
#else
    typealias ExpectedType0 = LazyMapCollection<LazyFilterCollection<C>, C.Element>
    typealias ExpectedType1 = LazyFilterCollection<LazyMapCollection<C, C.Element>>
    typealias ExpectedType2 = LazyMapCollection<LazyFilterCollection<LazyMapCollection<LazyFilterCollection<LazyMapCollection<C, C.Element?>>, C.Element>>, C.Element>
    typealias ExpectedType3 = LazyMapCollection<LazyFilterCollection<LazyMapCollection<LazyFilterCollection<LazyMapCollection<C, C.Element>>, C.Element?>>, C.Element>
#endif
    expectType(ExpectedType0.self, &result0)
    expectType(ExpectedType1.self, &result1)
    expectType(ExpectedType2.self, &result2)
    expectType(ExpectedType3.self, &result3)
  }
  foldingLevels(Array(0..<10))

  func backwardCompatible<C: Collection>(_ xs: C) {
    typealias ExpectedType0 = LazyMapCollection<LazyFilterCollection<C>, C.Element>
    typealias ExpectedType1 = LazyFilterCollection<LazyMapCollection<C, C.Element>>
    var result0: ExpectedType0 = xs.lazy.filter({ _ in true }).map({ $0 })
    var result1: ExpectedType1 = xs.lazy.map({ $0 }).filter({ _ in true })
    expectType(ExpectedType0.self, &result0)
    expectType(ExpectedType1.self, &result1)
  }
  backwardCompatible(Array(0..<10))
}

runAllTests()
