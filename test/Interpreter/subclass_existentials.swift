//===--- subclass_existentials.swift --------------------------------------===//
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
//
// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test

import StdlibUnittest

protocol P {
  init(protocolInit: ())

  func protocolMethodReturnsSelf() -> Self
  static func staticProtocolMethodReturnsSelf() -> Self

  var protocolProperty: Int { get set }
  static var staticProtocolProperty: Int { get set }

  subscript<U : Hashable>(protocolKey protocolKey: U) -> Int { get set }
}

protocol R {}

extension R {
  func extensionMethod() -> Self {
    return self
  }
}

var globalVar = 8

class Base<T> : R {
  var x: T
  var y: T
  var token = LifetimeTracked(0)
  var dict: [AnyHashable : T] = [:]

  required init(x: T, y: T) {
    self.x = x
    self.y = y
  }

  func classMethodReturnsSelf() -> Self {
    return self
  }

  func finalClassMethod() -> (T, T) {
    return (x, y)
  }

  func nonFinalClassMethod() -> (T, T) {
    return (x, y)
  }

  var propertyToOverride: Int = -8

  class var classPropertyToOverride: Int {
    get {
      return globalVar
    }
    set {
      globalVar = newValue
    }
  }

  subscript<U : Hashable>(key: U) -> T {
    get {
      return dict[key]!
    }
    set {
      self.dict[key] = newValue
    }
  }
}

class Derived : Base<Int>, P {
  required init(x: Int, y: Int) {
    super.init(x: x, y: y)
  }

  override func nonFinalClassMethod() -> (Int, Int) {
    return (y, x)
  }

  override var propertyToOverride: Int {
    get {
      return super.propertyToOverride * 2
    }
    set {
      super.propertyToOverride = newValue / 2
    }
  }

  class override var classPropertyToOverride: Int {
    get {
      return super.classPropertyToOverride * 2
    }
    set {
      super.classPropertyToOverride = newValue / 2
    }
  }

  convenience required init(protocolInit: ()) {
    self.init(x: 10, y: 20)
  }

  func protocolMethodReturnsSelf() -> Self {
    return self
  }

  static func staticProtocolMethodReturnsSelf() -> Self {
    return self.init(x: -1000, y: -2000)
  }

  var protocolProperty: Int = 100

  static var staticProtocolProperty: Int = 2000

  subscript<U : Hashable>(protocolKey protocolKey: U) -> Int {
    get {
      return dict[protocolKey]!
    }
    set {
      self.dict[protocolKey] = newValue
    }
  }
}

protocol Q : class {}

var SubclassExistentialsTestSuite = TestSuite("SubclassExistentials")

SubclassExistentialsTestSuite.test("Metadata instantiation") {
  expectTrue((Base<String> & Base<String>).self == Base<String>.self)
  expectTrue((Base<String> & Any).self == Base<String>.self)

  expectTrue((Base<Int> & Q).self == (Q & Base<Int>).self)

  expectTrue((Base<Int> & P & Q).self == (P & Base<Int> & Q).self)
  expectTrue((P & Q & Base<Int>).self == (Q & Base<Int> & P).self)

  expectTrue((P & Q).self == (P & Q & AnyObject).self)
  expectTrue((P & Q).self == (Q & P & AnyObject).self)
  expectTrue((Base<Int> & Q).self == (Q & Base<Int> & AnyObject).self)

  expectFalse((R & AnyObject).self == R.self)
}

SubclassExistentialsTestSuite.test("Metadata to string") {
  expectEqual("Base<Int> & P", String(describing: (Base<Int> & P).self))
  expectEqual("Base<Int> & P & Q", String(describing: (Base<Int> & P & Q).self))
}

SubclassExistentialsTestSuite.test("Call instance methods") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    // Basic method calls
    expectTrue(value === value.classMethodReturnsSelf())
    expectTrue((123, 321) == value.finalClassMethod())
    expectTrue((321, 123) == value.nonFinalClassMethod())

    expectTrue(value === value.extensionMethod())

    // Partial application
    do {
      let fn = value.classMethodReturnsSelf
      expectTrue(value === fn())
    }

    do {
      let fn = value.finalClassMethod
      expectTrue((123, 321) == fn())
    }

    do {
      let fn = value.nonFinalClassMethod
      expectTrue((321, 123) == fn())
    }
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Access instance properties") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    expectEqual(-16, value.propertyToOverride)
    value.propertyToOverride += 4
    expectEqual(-12, value.propertyToOverride)
    value.propertyToOverride += 1
    expectEqual(-10, value.propertyToOverride)
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Access subscript") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    value[1] = 2
    value["hi"] = 20

    expectEqual(2, value[1])
    expectEqual(20, value["hi"])

    value[1] += 1
    value["hi"] += 1

    expectEqual(3, value[1])
    expectEqual(21, value["hi"])
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Call static methods") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)
    let metatype: (Base<Int> & P).Type = type(of: value)

    let newValue = metatype.init(x: 256, y: 512)
    expectTrue(newValue === newValue.classMethodReturnsSelf())
    expectTrue((256, 512) == newValue.finalClassMethod())
    expectTrue((512, 256) == newValue.nonFinalClassMethod())

    do {
      let fn = metatype.init(x:y:)
      let newValue = fn(1, 2)
      expectTrue((1, 2) == newValue.finalClassMethod())
    }
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Access static properties") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    expectEqual(16, type(of: value).classPropertyToOverride)
    type(of: value).classPropertyToOverride += 4
    expectEqual(20, type(of: value).classPropertyToOverride)
    type(of: value).classPropertyToOverride += 1
    expectEqual(20, type(of: value).classPropertyToOverride)
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Call protocol instance methods") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    // Basic method calls
    expectTrue(value === value.protocolMethodReturnsSelf())

    // Partial application
    do {
      let fn = value.protocolMethodReturnsSelf
      expectTrue(value === fn())
    }
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Access protocol instance properties") {
  do {
    var value: Base<Int> & P = Derived(x: 123, y: 321)

    expectEqual(100, value.protocolProperty)
    value.protocolProperty += 1
    expectEqual(101, value.protocolProperty)
  }

  expectEqual(0, LifetimeTracked.instances)
}

SubclassExistentialsTestSuite.test("Access protocol subscript") {
  do {
    var value: Base<Int> & P = Derived(x: 123, y: 321)

    value[protocolKey: 1] = 2
    value[protocolKey: "hi"] = 20

    expectEqual(2, value[protocolKey: 1])
    expectEqual(20, value[protocolKey: "hi"])

    value[protocolKey: 1] += 1
    value[protocolKey: "hi"] += 1

    expectEqual(3, value[protocolKey: 1])
    expectEqual(21, value[protocolKey: "hi"])
  }

  expectEqual(0, LifetimeTracked.instances)
}

// Note: in optimized builds, these tests will get optimized down and
// exercise a totally different code path. That's fine.
SubclassExistentialsTestSuite.test("Scalar downcast to subclass existential") {
  do {
    let baseInt: Base<Int> = Derived(x: 123, y: 321)
    let derived = baseInt as? (Base<Int> & P)

    expectEqual(123, derived!.x)
    expectEqual(321, derived!.y)
  }

  do {
    let p: P = Derived(x: 123, y: 321)
    let result = p as? (Base<Int> & P)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  do {
    let r: R = Derived(x: 123, y: 321)
    let result = r as? (Base<Int> & P)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  do {
    let baseInt: Base<Int> = Derived(x: 123, y: 321)
    let result = baseInt as? (Base<Int> & P)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  expectEqual(0, LifetimeTracked.instances)
}

func cast<T, U>(_ t: T, to: U.Type) -> U? {
  return t as? U
}

// Note: in optimized builds, these tests will get optimized down and
// exercise a totally different code path. That's fine.
SubclassExistentialsTestSuite.test("Dynamic downcast to subclass existential") {
  do {
    let baseInt: Base<Int> = Derived(x: 123, y: 321)
    let derived = cast(baseInt, to: (Base<Int> & P).self)

    expectEqual(123, derived!.x)
    expectEqual(321, derived!.y)
  }

  do {
    let p: P = Derived(x: 123, y: 321)
    let result = cast(p, to: (Base<Int> & P).self)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  do {
    let r: R = Derived(x: 123, y: 321)
    let result = cast(r, to: (Base<Int> & P).self)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  do {
    let baseInt: Base<Int> = Derived(x: 123, y: 321)
    let result = cast(baseInt, to: (Base<Int> & P).self)

    expectEqual(123, result!.x)
    expectEqual(321, result!.y)
  }

  expectEqual(0, LifetimeTracked.instances)
}

class ConformsToP : P {
  var token = LifetimeTracked(0)

  required init(protocolInit: ()) {}

  func protocolMethodReturnsSelf() -> Self {
    return self
  }

  static func staticProtocolMethodReturnsSelf() -> Self {
    return self.init(protocolInit: ())
  }

  var protocolProperty: Int = 100

  static var staticProtocolProperty: Int = 2000

  subscript<U : Hashable>(protocolKey protocolKey: U) -> Int {
    get {
      return 0
    }
    set {}
  }
}

SubclassExistentialsTestSuite.test("Failing scalar downcast to subclass existential") {
  do {
    let baseInt: Base<Int> = Base<Int>(x: 123, y: 321)

    expectNil(baseInt as? (Base<Int> & P))
    expectFalse(baseInt is (Base<Int> & P))
  }

  do {
    let r: R = Base<Int>(x: 123, y: 321)

    expectNil(r as? (Base<Int> & P))
    expectFalse(r is (Base<Int> & P))
  }

  do {
    let conformsToP = ConformsToP(protocolInit: ())

    expectNil(conformsToP as? (Base<Int> & P))
    expectFalse(conformsToP is (Base<Int> & P))
  }
}

SubclassExistentialsTestSuite.test("Failing dynamic downcast to subclass existential") {
  do {
    let baseInt: Base<Int> = Base<Int>(x: 123, y: 321)

    expectNil(cast(baseInt, to: (Base<Int> & P).self))
  }

  do {
    let r: R = Base<Int>(x: 123, y: 321)

    expectNil(cast(r, to: (Base<Int> & P).self))
  }

  do {
    let conformsToP = ConformsToP(protocolInit: ())

    expectNil(cast(conformsToP, to: (Base<Int> & P).self))
  }
}

// This test triggers https://github.com/apple/swift/issues/43427
// (rdar://problem/25318716) on macOS 10.9 and iOS 7. Disable it for now when
// testing on those versions.
if #available(macOS 10.10, iOS 8, *) {
  runAllTests()
} else {
  runNoTests()
}
