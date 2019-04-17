//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark tests AnyHashable's initializer that needs to dynamically
// upcast the instance to the type that introduces the Hashable
// conformance.

import TestsUtils

// 23% _swift_dynamicCast
// 23% _swift_release_
// 18% _swift_stdlib_makeAnyHashableUsingDefaultRepresentation
// 11% _swift_stdlib_makeAnyHashableUpcastingToHashableBaseType
// 16% _swift_retain_[n]
//  5% swift_conformsToProtocol
public var AnyHashableWithAClass = BenchmarkInfo(
  name: "AnyHashableWithAClass",
  runFunction: run_AnyHashableWithAClass,
  tags: [.abstraction, .runtime, .cpubench],
  legacyFactor: 500
)

class TestHashableBase : Hashable {
  var value: Int
  init(_ value: Int) {
    self.value = value
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func == (
    lhs: TestHashableBase,
    rhs: TestHashableBase
  ) -> Bool {
    return lhs.value == rhs.value
  }
}

class TestHashableDerived1 : TestHashableBase {}
class TestHashableDerived2 : TestHashableDerived1 {}
class TestHashableDerived3 : TestHashableDerived2 {}
class TestHashableDerived4 : TestHashableDerived3 {}
class TestHashableDerived5 : TestHashableDerived4 {}

@inline(never)
public func run_AnyHashableWithAClass(_ N: Int) {
  let c = TestHashableDerived5(10)
  for _ in 0...(N*1000) {
    _ = AnyHashable(c)
  }
}
