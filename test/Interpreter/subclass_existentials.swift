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
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out -Xfrontend -enable-experimental-subclass-existentials
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
//

import StdlibUnittest

// FIXME: Various Sema and SILGen crashes if this is not ': class'
protocol P : class {
  init(protocolInit: ())

  func protocolMethodReturnsSelf() -> Self
  static func staticProtocolMethodReturnsSelf() -> Self

  var protocolProperty: Int { get set }
  static var staticProtocolProperty: Int { get set }

  subscript<U : Hashable>(protocolKey protocolKey: U) -> Int { get set }
}

// TODO: Member access on R that Base<T> concretely conforms to

protocol R {}

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
  convenience required init(protocolInit: ()) {
    self.init(x: 10, y: 20)
  }
  
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

// Note: we write ((A) & B) if A is generic to work around a limitation
// in preCheckExpression().

SubclassExistentialsTestSuite.test("Metadata instantiation") {
  expectTrue(((Base<String>) & Base<String>).self == Base<String>.self)
  expectTrue(((Base<String>) & Any).self == Base<String>.self)

  expectTrue(((Base<Int>) & Q).self == (Q & Base<Int>).self)

  expectTrue(((Base<Int>) & P & Q).self == (P & (Base<Int>) & Q).self)
  expectTrue((P & Q & (Base<Int>)).self == (Q & (Base<Int>) & P).self)

  // For now...
  expectFalse((P & Q).self == (P & Q & AnyObject).self)
  expectFalse((P & Q).self == (Q & P & AnyObject).self)
  expectFalse(((Base<Int>) & Q).self == (Q & (Base<Int>) & AnyObject).self)
}

// FIXME: Not implemented yet

/*
SubclassExistentialsTestSuite.test("Metadata to string") {
  expectEqual("Base<Int> & P", String(describing: ((Base<Int>) & P).self))
  expectEqual("Base<Int> & P & Q", String(describing: ((Base<Int>) & P & Q).self))
}
*/

SubclassExistentialsTestSuite.test("Call instance methods") {
  do {
    let value: Base<Int> & P = Derived(x: 123, y: 321)

    // Basic method calls
    expectTrue(value === value.classMethodReturnsSelf())
    expectTrue((123, 321) == value.finalClassMethod())
    expectTrue((321, 123) == value.nonFinalClassMethod())

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

runAllTests()
