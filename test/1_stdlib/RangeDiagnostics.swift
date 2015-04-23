//===--- RangeDiagnostics.swift -------------------------------------------===//
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
// RUN: %target-parse-verify-swift

func assertCollection<C: CollectionType>(_: C) {}
assertCollection(0..<10)

// The point of this test is to check that we don't allow indexing
// Range<Int> et al with Int outside a generic context, to prevent the
// problem that r[0] will be invalid when 0 is not contained int he
// range.

// Many of these error messages are terrible.  If the test starts
// failing because the error message improves, obviously, update the
// test!

let r0: Range = 10..<100        
let r1: Range = UInt(10)..<100
let r2: Range = 10...100
let r3: Range = UInt(10)...100
r0[0]       // expected-error {{cannot subscript a value of type 'Range<Int>' with an index of type 'Int'}}
r1[UInt(0)] // expected-error {{cannot subscript a value of type 'Range<UInt>' with an index of type 'UInt'}}
r1[0]       // expected-error {{cannot subscript a value of type 'Range<UInt>' with an index of type 'Int'}}
r2[0]       // expected-error {{cannot subscript a value of type 'Range<Int>' with an index of type 'Int'}}
r3[0]       // expected-error {{cannot subscript a value of type 'Range<UInt>' with an index of type 'Int'}}
r0[0..<4]   // expected-error {{cannot subscript a value of type 'Range<Int>' with an index of type 'Range<Int>'}}
r1[0..<4]   // expected-error {{cannot subscript a value of type 'Range<UInt>' with an index of type 'Range<Int>'}}
r2[0..<4]   // expected-error {{cannot subscript a value of type 'Range<Int>' with an index of type 'Range<Int>'}}
r3[0..<4]   // expected-error {{cannot subscript a value of type 'Range<UInt>' with an index of type 'Range<Int>'}}
(10..<100)[0]           // expected-error {{HalfOpenInterval<T>' does not have a member named 'subscript'}}
(UInt(10)...100)[0..<4] // expected-error {{cannot subscript a value of type '(Range<UInt>)' with an index of type 'Range<Int>'}}

