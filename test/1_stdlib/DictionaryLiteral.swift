//===--- DictionaryLiteral.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import SwiftExperimental
import Foundation
import StdlibUnittest

var strings: DictionaryLiteral = ["a": "1", "b": "Foo"]
expectType(DictionaryLiteral<String, String>.self, &strings)

var stringNSStringLiteral: DictionaryLiteral = [
  "a": "1", "b": "Foo" as NSString]
expectType(DictionaryLiteral<String, NSString>.self, &stringNSStringLiteral)

let aString = "1"
let anNSString = "Foo" as NSString
var stringNSStringLet: DictionaryLiteral = [ "a": aString, "b": anNSString]
expectType(DictionaryLiteral<String, NSString>.self, &stringNSStringLet)

var hetero1: DictionaryLiteral = ["a": 1, "b": "Foo" as NSString]
expectType(DictionaryLiteral<String, NSObject>.self, &hetero1)

var hetero2: DictionaryLiteral = ["a": 1, "b": "Foo"]
expectType(DictionaryLiteral<String, NSObject>.self, &hetero2)
