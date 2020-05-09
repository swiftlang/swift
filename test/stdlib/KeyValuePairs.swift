//===--- KeyValuePairs.swift ------------------------------------------===//
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

import Foundation
import StdlibUnittest


// Check that the generic parameters are called 'Key' and 'Value'.
protocol TestProtocol1 {}

extension KeyValuePairs where Key : TestProtocol1, Value : TestProtocol1 {
  var _keyAndValueAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

func checkAssociatedTypes() {
  typealias Subject = KeyValuePairs<MinimalHashableValue, OpaqueValue<Int>>
  expectRandomAccessCollectionAssociatedTypes(
    collectionType: Subject.self,
    iteratorType: IndexingIterator<Subject>.self,
    subSequenceType: Slice<Subject>.self,
    indexType: Int.self,
    indicesType: CountableRange<Int>.self)
}

var strings: KeyValuePairs = ["a": "1", "b": "Foo"]
expectType(KeyValuePairs<String, String>.self, &strings)

var stringNSStringLiteral: KeyValuePairs = [
  "a": "1", "b": "Foo" as NSString]
expectType(KeyValuePairs<String, NSString>.self, &stringNSStringLiteral)

let aString = "1"
let anNSString = "Foo" as NSString
var stringNSStringLet: KeyValuePairs = [ "a": aString as NSString, "b": anNSString]
expectType(KeyValuePairs<String, NSString>.self, &stringNSStringLet)

var hetero: KeyValuePairs = ["a": 1 as NSNumber, "b": "Foo" as NSString]
expectType(KeyValuePairs<String, NSObject>.self, &hetero)
