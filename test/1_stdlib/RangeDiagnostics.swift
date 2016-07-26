//===--- RangeDiagnostics.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-parse-verify-swift

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
    let r1: Range<C>       = v...v // expected-error {{no '...' candidates produce the expected contextual result type 'Range<C>'}} expected-note {{}}
    let r2: ClosedRange<C> = v..<v // expected-error {{no '..<' candidates produce the expected contextual result type 'ClosedRange<C>'}} expected-note {{}}
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
    let r1: Range<S>       = v...v // expected-error {{no '...' candidates produce the expected contextual result type 'Range<S>'}} expected-note {{}}
    let r2: ClosedRange<S> = v..<v // expected-error {{no '..<' candidates produce the expected contextual result type 'ClosedRange<S>'}} expected-note {{}}
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
    let range: Range<S> = v..<v
  }
  do {
    let range: ClosedRange<S> = v...v
  }
  do {
    let r1: Range<S>       = v...v // expected-error {{no '...' candidates produce the expected contextual result type 'Range<S>'}} expected-note {{}}
    let r2: ClosedRange<S> = v..<v // expected-error {{no '..<' candidates produce the expected contextual result type 'ClosedRange<S>'}} expected-note {{}}
    let r3: CountableRange<S>       = v...v // expected-error {{no '...' candidates produce the expected contextual result type 'CountableRange<S>'}} expected-note {{}}
    let r4: CountableClosedRange<S> = v..<v // expected-error {{no '..<' candidates produce the expected contextual result type 'CountableClosedRange<S>'}} expected-note {{}}
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
    r2[0]       // expected-error {{cannot convert value of type 'Int' to expected argument type 'Range<ClosedRangeIndex<_>>'}}
    r3[0]       // expected-error {{cannot convert value of type 'Int' to expected argument type 'Range<ClosedRangeIndex<_>>'}}

    r0[UInt(0)] // expected-error {{cannot subscript a value of type 'CountableRange<Int>' with an index of type 'UInt'}} expected-note {{overloads for 'subscript' exist}}
    r1[UInt(0)] // expected-error {{ambiguous use of 'subscript'}}
    r2[UInt(0)] // expected-error {{cannot convert value of type 'UInt' to expected argument type 'Range<ClosedRangeIndex<_>>'}}
    r3[UInt(0)] // expected-error {{cannot convert value of type 'UInt' to expected argument type 'Range<ClosedRangeIndex<_>>'}}

    r0[0..<4]   // expected-error {{ambiguous use of 'subscript'}}
    r1[0..<4]   // expected-error {{ambiguous use of 'subscript'}}
    r2[0..<4]   // expected-error {{cannot convert call result type 'CountableRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    r3[0..<4]   // expected-error {{cannot convert call result type 'CountableRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    (10..<100)[0]           // expected-error {{ambiguous use of 'subscript'}}
    (UInt(10)...100)[0..<4] // expected-error {{cannot convert call result type 'CountableRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}

    r0[0...4]   // expected-error {{ambiguous use of 'subscript'}}
    r1[0...4]   // expected-error {{ambiguous use of 'subscript'}}
    r2[0...4]   // expected-error {{cannot convert call result type 'CountableClosedRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    r3[0...4]   // expected-error {{cannot convert call result type 'CountableClosedRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    (10...100)[0...4] // expected-error {{cannot convert call result type 'CountableClosedRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    (UInt(10)...100)[0...4] // expected-error {{cannot convert call result type 'CountableClosedRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}

    r0[r0]      // expected-error {{ambiguous use of 'subscript'}}
    r0[r1]      // expected-error {{ambiguous reference to member 'subscript'}}
    r0[r2]      // expected-error {{ambiguous use of 'subscript'}}
    r0[r3]      // expected-error {{ambiguous reference to member 'subscript'}}

    r1[r0]      // expected-error {{ambiguous reference to member 'subscript'}}
    r1[r1]      // expected-error {{ambiguous use of 'subscript'}}
    r1[r2]      // expected-error {{ambiguous reference to member 'subscript'}}
    r1[r3]      // expected-error {{ambiguous use of 'subscript'}}

    r2[r0]      // expected-error {{cannot convert value}}
    r2[r1]      // expected-error {{cannot convert value}}
    r2[r2]      // expected-error {{cannot convert value}}
    r2[r3]      // expected-error {{cannot convert value}}

    r3[r0]      // expected-error {{cannot convert value}}
    r3[r1]      // expected-error {{cannot convert value}}
    r3[r2]      // expected-error {{cannot convert value}}
    r3[r3]      // expected-error {{cannot convert value}}
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
    (UInt(10)...100)[0..<4] // expected-error {{cannot convert call result type}}

    r0[0...4]   // expected-error {{type 'Range<Int>' has no subscript members}}
    r1[0...4]   // expected-error {{type 'Range<UInt>' has no subscript members}}
    r2[0...4]   // expected-error {{type 'ClosedRange<Int>' has no subscript members}}
    r3[0...4]   // expected-error {{type 'ClosedRange<UInt>' has no subscript members}}
    (10...100)[0...4] // expected-error {{cannot convert call result type 'CountableClosedRange<_>' to expected type 'Range<ClosedRangeIndex<_>>'}}
    (UInt(10)...100)[0...4] // expected-error {{cannot convert call result type}}

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
