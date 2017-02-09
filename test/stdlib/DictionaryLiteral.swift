//===--- DictionaryLiteral.swift ------------------------------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import SwiftExperimental
import Foundation
import StdlibUnittest


// Check that the generic parameters are called 'Key' and 'Value'.
protocol TestProtocol1 {}

extension DictionaryLiteral where Key : TestProtocol1, Value : TestProtocol1 {
  var _keyAndValueAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

func checkAssociatedTypes() {
  typealias Subject = DictionaryLiteral<MinimalHashableValue, OpaqueValue<Int>>
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Subject.self,
    iteratorType: IndexingIterator<Subject>.self,
    subSequenceType: RandomAccessSlice<Subject>.self,
    indexType: Int.self,
    indexDistanceType: Int.self,
    indicesType: CountableRange<Int>.self)
}

var strings: DictionaryLiteral = ["a": "1", "b": "Foo"]
expectType(DictionaryLiteral<String, String>.self, &strings)

var stringNSStringLiteral: DictionaryLiteral = [
  "a": "1", "b": "Foo" as NSString]
expectType(DictionaryLiteral<String, NSString>.self, &stringNSStringLiteral)

let aString = "1"
let anNSString = "Foo" as NSString
var stringNSStringLet: DictionaryLiteral = [ "a": aString as NSString, "b": anNSString]
expectType(DictionaryLiteral<String, NSString>.self, &stringNSStringLet)

var hetero1: DictionaryLiteral = ["a": 1 as NSNumber, "b": "Foo" as NSString]
expectType(DictionaryLiteral<String, NSObject>.self, &hetero1)

var hetero2: DictionaryLiteral = ["a": 1 as NSNumber, "b": "Foo" as NSString]
expectType(DictionaryLiteral<String, NSObject>.self, &hetero2)
