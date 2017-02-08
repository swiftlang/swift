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

class TestHashableBase : Hashable {
  var value: Int
  init(_ value: Int) {
    self.value = value
  }
  var hashValue: Int {
    return value
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
  for _ in 0...(N*500000) {
    _ = AnyHashable(c)
  }
}

