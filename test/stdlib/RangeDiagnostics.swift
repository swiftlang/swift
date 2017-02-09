//===--- RangeDiagnostics.swift -------------------------------------------===//
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
// RUN: %target-typecheck-verify-swift

import StdlibUnittest

func typeInference_Comparable<C : Comparable>(v: C) {
  do {
    var range = v..<v
    expectType(Range<C>.self, &range)
  }
  do {
    var range = v...v
    expectType(ClosedRange<C>.self, &range)
  }
  do {
    let r1: Range<C>       = v...v // expected-error {{cannot convert value of type 'ClosedRange<C>' to specified type 'Range<C>'}}
    let r2: ClosedRange<C> = v..<v // expected-error {{cannot convert value of type 'Range<C>' to specified type 'ClosedRange<C>'}}
    let r3: CountableRange<C>       = v..<v // expected-error {{type 'C' does not conform to protocol '_Strideable'}}
    let r4: CountableClosedRange<C> = v...v // expected-error {{type 'C' does not conform to protocol '_Strideable'}}
    let r5: CountableRange<C>       = v...v // expected-error {{type 'C' does not conform to protocol '_Strideable'}}
    let r6: CountableClosedRange<C> = v..<v // expected-error {{type 'C' does not conform to protocol '_Strideable'}}
  }
}

func typeInference_Strideable<S : Strideable>(v: S) {
  do {
    var range = v..<v
    expectType(Range<S>.self, &range)
  }
  do {
    var range = v...v
    expectType(ClosedRange<S>.self, &range)
  }
  do {
    let r1: Range<S>       = v...v // expected-error {{cannot convert value of type 'ClosedRange<S>' to specified type 'Range<S>'}}
    let r2: ClosedRange<S> = v..<v // expected-error {{cannot convert value of type 'Range<S>' to specified type 'ClosedRange<S>'}}
    let r3: CountableRange<S>       = v..<v // expected-error {{type 'S.Stride' does not conform to protocol 'SignedInteger'}}
    let r4: CountableClosedRange<S> = v...v // expected-error {{type 'S.Stride' does not conform to protocol 'SignedInteger'}}
    let r5: CountableRange<S>       = v...v // expected-error {{type 'S.Stride' does not conform to protocol 'SignedInteger'}}
    let r6: CountableClosedRange<S> = v..<v // expected-error {{type 'S.Stride' does not conform to protocol 'SignedInteger'}}
  }
}

func typeInference_StrideableWithSignedIntegerStride<S : Strideable>(v: S)
  where S.Stride : SignedInteger {
  do {
    var range = v..<v
    expectType(CountableRange<S>.self, &range)
  }
  do {
    var range = v...v
    expectType(CountableClosedRange<S>.self, &range)
  }
  do {
    let _: Range<S> = v..<v
  }
  do {
    let _: ClosedRange<S> = v...v
  }
  do {
    let _: Range<S>       = v...v // expected-error {{cannot convert value of type 'CountableClosedRange<S>' to specified type 'Range<S>'}}
    let _: ClosedRange<S> = v..<v // expected-error {{cannot convert value of type 'CountableRange<S>' to specified type 'ClosedRange<S>'}}
    let _: CountableRange<S>       = v...v // expected-error {{cannot convert value of type 'CountableClosedRange<S>' to specified type 'CountableRange<S>'}}
    let _: CountableClosedRange<S> = v..<v // expected-error {{cannot convert value of type 'CountableRange<S>' to specified type 'CountableClosedRange<S>'}}
  }
}

// Check how type inference works with a few commonly used types.
func typeInference_commonTypes() {
  // ---------------------------------------------
  // operator '..<'
  // ---------------------------------------------
  do {
    var range = 1..<10
    expectType(CountableRange<Int>.self, &range)
  }
  do {
    var range = UInt(1)..<10
    expectType(CountableRange<UInt>.self, &range)
  }
  do {
    var range = Int8(1)..<10
    expectType(CountableRange<Int8>.self, &range)
  }
  do {
    var range = UInt8(1)..<10
    expectType(CountableRange<UInt8>.self, &range)
  }
  do {
    var range = 1.0..<10.0
    expectType(Range<Double>.self, &range)
  }
  do {
    var range = Float(1.0)..<10.0
    expectType(Range<Float>.self, &range)
  }
  do {
    var range = "a"..<"z"
    expectType(Range<String>.self, &range)
  }
  do {
    var range = Character("a")..<"z"
    expectType(Range<Character>.self, &range)
  }
  do {
    var range = UnicodeScalar("a")..<"z"
    expectType(Range<UnicodeScalar>.self, &range)
  }
  do {
    let s = ""
    var range = s.startIndex..<s.endIndex
    expectType(Range<String.Index>.self, &range)
  }

  // ---------------------------------------------
  // operator '...'
  // ---------------------------------------------
  do {
    var range = 1...10
    expectType(CountableClosedRange<Int>.self, &range)
  }
  do {
    var range = UInt(1)...10
    expectType(CountableClosedRange<UInt>.self, &range)
  }
  do {
    var range = Int8(1)...10
    expectType(CountableClosedRange<Int8>.self, &range)
  }
  do {
    var range = UInt8(1)...10
    expectType(CountableClosedRange<UInt8>.self, &range)
  }
  do {
    var range = 1.0...10.0
    expectType(ClosedRange<Double>.self, &range)
  }
  do {
    var range = Float(1.0)...10.0
    expectType(ClosedRange<Float>.self, &range)
  }
  do {
    var range = "a"..."z"
    expectType(ClosedRange<String>.self, &range)
  }
  do {
    var range = Character("a")..."z"
    expectType(ClosedRange<Character>.self, &range)
  }
  do {
    var range = UnicodeScalar("a")..."z"
    expectType(ClosedRange<UnicodeScalar>.self, &range)
  }
  do {
    let s = ""
    var range = s.startIndex...s.endIndex
    expectType(ClosedRange<String.Index>.self, &range)
  }
}

func disallowSubscriptingOnIntegers() {
  // FIXME: swift-3-indexing-model: decide what to do with the following QoI.
  // The previous implementation was imposing an ABI burden.

  // The point of this test is to check that we don't allow indexing
  // Range<Int> et al with Int outside a generic context, to prevent the
  // problem that r[0] will be invalid when 0 is not contained int he
  // range.

  // Many of these error messages are terrible.  If the test starts
  // failing because the error message improves, obviously, update the
  // test!
  do {
    var r0 = 10..<100
    var r1 = UInt(10)..<100
    var r2 = 10...100
    var r3 = UInt(10)...100
    expectType(CountableRange<Int>.self, &r0)
    expectType(CountableRange<UInt>.self, &r1)
    expectType(CountableClosedRange<Int>.self, &r2)
    expectType(CountableClosedRange<UInt>.self, &r3)

    r0[0]       // expected-error {{ambiguous use of 'subscript'}}
    r1[0]       // expected-error {{ambiguous use of 'subscript'}}
    r2[0]       // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'Int'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r3[0]       // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'Int'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r0[UInt(0)] // expected-error {{ambiguous reference to member 'subscript'}}
    r1[UInt(0)] // expected-error {{ambiguous use of 'subscript'}}
    r2[UInt(0)] // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'UInt'}}
    // expected-note@-1 {{overloads for 'subscript' exist}}
    r3[UInt(0)] // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'UInt'}}
    // expected-note@-1 {{overloads for 'subscript' exist}}

    r0[0..<4]   // expected-error {{ambiguous use of 'subscript'}}
    r1[0..<4]   // expected-error {{ambiguous use of 'subscript'}}
    r2[0..<4]   // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript' exist}}
    r3[0..<4]   // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript' exist}}
    (10..<100)[0]           // expected-error {{ambiguous use of 'subscript'}}
    (UInt(10)...100)[0..<4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r0[0...4]   // expected-error {{ambiguous use of 'subscript'}}
    r1[0...4]   // expected-error {{ambiguous use of 'subscript'}}
    r2[0...4]   // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r3[0...4]   // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    (10...100)[0...4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    (UInt(10)...100)[0...4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r0[r0]      // expected-error {{ambiguous use of 'subscript'}}
    r0[r1]      // expected-error {{ambiguous reference to member 'subscript'}}
    r0[r2]      // expected-error {{ambiguous use of 'subscript'}}
    r0[r3]      // expected-error {{ambiguous reference to member 'subscript'}}

    r1[r0]      // expected-error {{ambiguous reference to member 'subscript'}}
    r1[r1]      // expected-error {{ambiguous use of 'subscript'}}
    r1[r2]      // expected-error {{ambiguous reference to member 'subscript'}}
    r1[r3]      // expected-error {{ambiguous use of 'subscript'}}

    r2[r0]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r2[r1]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableRange<UInt>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r2[r2]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r2[r3]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableClosedRange<UInt>'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r3[r0]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r3[r1]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableRange<UInt>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r3[r2]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    r3[r3]      // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableClosedRange<UInt>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
  }

  do {
    let r0: Range = 10..<100
    let r1: Range = UInt(10)..<100
    let r2: ClosedRange = 10...100
    let r3: ClosedRange = UInt(10)...100
    r0[0]       // expected-error {{type 'Range<Int>' has no subscript members}}
    r1[0]       // expected-error {{type 'Range<UInt>' has no subscript members}}
    r2[0]       // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r3[0]       // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}

    r0[UInt(0)] // expected-error {{type 'Range<Int>' has no subscript members}}
    r1[UInt(0)] // expected-error {{type 'Range<UInt>' has no subscript members}}
    r2[UInt(0)] // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r3[UInt(0)] // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}

    r0[0..<4]   // expected-error {{type 'Range<Int>' has no subscript members}}
    r1[0..<4]   // expected-error {{type 'Range<UInt>' has no subscript members}}
    r2[0..<4]   // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r3[0..<4]   // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    (10..<100)[0]           // expected-error {{ambiguous use of 'subscript'}}
    (UInt(10)...100)[0..<4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r0[0...4]   // expected-error {{type 'Range<Int>' has no subscript members}}
    r1[0...4]   // expected-error {{type 'Range<UInt>' has no subscript members}}
    r2[0...4]   // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r3[0...4]   // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    (10...100)[0...4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<Int>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}
    (UInt(10)...100)[0...4] // expected-error {{cannot subscript a value of type 'CountableClosedRange<UInt>' with an index of type 'CountableClosedRange<Int>'}}
    // expected-note@-1 {{overloads for 'subscript'}}

    r0[r0]      // expected-error {{type 'Range<Int>' has no subscript members}}
    r0[r1]      // expected-error {{type 'Range<Int>' has no subscript members}}
    r0[r2]      // expected-error {{type 'Range<Int>' has no subscript members}}
    r0[r3]      // expected-error {{type 'Range<Int>' has no subscript members}}

    r1[r0]      // expected-error {{type 'Range<UInt>' has no subscript members}}
    r1[r1]      // expected-error {{type 'Range<UInt>' has no subscript members}}
    r1[r2]      // expected-error {{type 'Range<UInt>' has no subscript members}}
    r1[r3]      // expected-error {{type 'Range<UInt>' has no subscript members}}

    r2[r0]      // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r2[r1]      // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r2[r2]      // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r2[r3]      // expected-error {{type 'ClosedRange<Int>' has no subscript members}}

    r3[r0]      // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    r3[r1]      // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    r3[r2]      // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    r3[r3]      // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
  }
}
