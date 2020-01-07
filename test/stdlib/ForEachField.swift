//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

struct TestStruct {
  var int = 0
  var double = 0.0
  var bool = false
}

struct GenericStruct<T> {
  var int = 0
  var first: T
  var second: T
}

enum TestEnum {
  case one
  case two
  case three(TestStruct)
}

class BaseClass {
  var superInt = 0
  init() {}
}

class TestClass: BaseClass {
  var int = 0
  var double = 0.0
  var bool = false
  override init() {}
}

class TestSubclass: TestClass {
  var strings: [String] = []
  override init() {}
}

class GenericClass<T, U>: BaseClass {
  var first: T
  var second: U
  
  init(_ t: T, _ u: U) {
    self.first = t
    self.second = u
  }
}

class GenericSubclass<V, W>: GenericClass<V, Bool> {
  var third: W
  
  init(_ v: V, _ w: W) {
    self.third = w
    super.init(v, false)
  }
}

func checkFields<T>(
  of type: T.Type,
  options: _EachFieldOptions = [],
  fields: [String: (Int, Any.Type)]
) {
  var count = 0
  
  _forEachField(of: T.self, options: options) {
    charPtr, offset, type in
    count += 1
    
    let fieldName = String(cString: charPtr)
    guard let (checkOffset, checkType) = fields[fieldName] else {
      expectTrue(false, "Unexpected field '\(fieldName)'")
      return true
    }
    
    expectEqual(checkOffset, offset)
    expectEqual(checkType, type)
    return true
  }

  expectEqual(fields.count, count)
}

protocol ExistentialProtocol {}

extension TestStruct: ExistentialProtocol {}
extension GenericStruct: ExistentialProtocol {}
extension GenericSubclass: ExistentialProtocol {}

extension ExistentialProtocol {
  static func doCheckFields(
    options: _EachFieldOptions = [],
    fields: [String: (Int, Any.Type)]
  ) {
    checkFields(of: Self.self, options: options, fields: fields)
  }
}

func checkFieldsAsExistential(
  of type: ExistentialProtocol.Type,
  options: _EachFieldOptions = [],
  fields: [String: (Int, Any.Type)]
) {
  type.doCheckFields(options: options, fields: fields)
}

//===----------------------------------------------------------------------===//

var tests = TestSuite("ForEachField")

tests.test("TestTuple") {
  checkFields(
    of: (Bool, Int).self,
    fields: [".0": (0, Bool.self), ".1": (8, Int.self)])
  
  checkFields(
    of: (a: Bool, b: Int).self,
    fields: ["a": (0, Bool.self), "b": (8, Int.self)])
}

tests.test("TestEnum") {
  checkFields(of: TestEnum.self, fields: [:])
}

tests.test("TestStruct") {
  checkFields(
    of: TestStruct.self,
    fields: [
      "int": (0, Int.self),
      "double": (8, Double.self),
      "bool": (16, Bool.self),
  ])

  checkFieldsAsExistential(
    of: TestStruct.self,
    fields: [
      "int": (0, Int.self),
      "double": (8, Double.self),
      "bool": (16, Bool.self),
  ])
  
  // Applying to struct type with .classType option fails
  expectFalse(_forEachField(of: TestStruct.self, options: .classType) {
    _, _, _ in true
  })
}

func checkGenericStruct<T>(_: T.Type) {
  checkFields(
    of: GenericStruct<T>.self,
    fields: [
      "int": (0, Int.self),
      "first": (8, T.self),
      "second": (8 + MemoryLayout<T>.stride, T.self),
  ])
  
  checkFieldsAsExistential(
    of: GenericStruct<T>.self,
    fields: [
      "int": (0, Int.self),
      "first": (8, T.self),
      "second": (8 + MemoryLayout<T>.stride, T.self),
  ])
}

tests.test("GenericStruct") {
  checkGenericStruct(Bool.self)
  checkGenericStruct(TestStruct.self)
  checkGenericStruct((TestStruct, TestClass, Int, Int).self)
}

tests.test("TestClass") {
  checkFields(
    of: TestClass.self, options: .classType,
    fields: [
      "superInt": (16, Int.self),
      "int": (24, Int.self),
      "double": (32, Double.self),
      "bool": (40, Bool.self),
  ])

  checkFields(
    of: TestSubclass.self, options: .classType,
    fields: [
      "superInt": (16, Int.self),
      "int": (24, Int.self),
      "double": (32, Double.self),
      "bool": (40, Bool.self),
      "strings": (48, Array<String>.self),
  ])
  
  checkFields(
    of: GenericSubclass<TestStruct, TestStruct>.self, options: .classType,
    fields: [
      "superInt": (16, Int.self),
      "first": (24, TestStruct.self),
      "second": (41, Bool.self),
      "third": (48, TestStruct.self),
  ])

  checkFields(
    of: GenericSubclass<Int, Never>.self, options: .classType,
    fields: [
      "superInt": (16, Int.self),
      "first": (24, Int.self),
      "second": (32, Bool.self),
      "third": (0, Never.self),
  ])
  
  checkFieldsAsExistential(
    of: GenericSubclass<TestStruct, TestStruct>.self, options: .classType,
    fields: [
      "superInt": (16, Int.self),
      "first": (24, TestStruct.self),
      "second": (41, Bool.self),
      "third": (48, TestStruct.self),
  ])

  // Applying to class type without .classType option fails
  expectFalse(_forEachField(of: TestClass.self) { _, _, _ in true })
}

runAllTests()
