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
    var range = v...
    expectType(PartialRangeFrom<C>.self, &range)
  }
  do {
    var range = ..<v
    expectType(PartialRangeUpTo<C>.self, &range)
  }
  do {
    var range = ...v
    expectType(PartialRangeThrough<C>.self, &range)
  }
  do {
    let _: Range<C>       = v...v // expected-error {{cannot convert value of type 'ClosedRange<C>' to specified type 'Range<C>'}}
    let _: ClosedRange<C> = v..<v // expected-error {{cannot convert value of type 'Range<C>' to specified type 'ClosedRange<C>'}}
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
    let r3: PartialRangeUpTo<S> = v... // expected-error {{cannot convert value of type 'PartialRangeFrom<S>' to specified type 'PartialRangeUpTo<S>'}}
    let r4: PartialRangeUpTo<S> = v... // expected-error {{cannot convert value of type 'PartialRangeFrom<S>' to specified type 'PartialRangeUpTo<S>'}}
    let r5: Range<S> = v..< // expected-error {{'..<' is not a postfix unary operator}}
  }
}

func typeInference_StrideableWithSignedIntegerStride<S : Strideable>(v: S)
  where S.Stride : SignedInteger {
  do {
    var range = v..<v
    expectType(Range<S>.self, &range)
  }
  do {
    var range = v...v
    expectType(ClosedRange<S>.self, &range)
  }
  do {
    var range = v...
    expectType(PartialRangeFrom<S>.self, &range)
  }
  do {
    let _: Range<S> = v..<v
  }
  do {
    let _: ClosedRange<S> = v...v
  }
  do {
    let _: Range<S>       = v...v // expected-error {{cannot convert value of type 'ClosedRange<S>' to specified type 'Range<S>'}}
    let _: ClosedRange<S> = v..<v // expected-error {{cannot convert value of type 'Range<S>' to specified type 'ClosedRange<S>'}}
    let _: Range<S>       = v...v // expected-error {{cannot convert value of type 'ClosedRange<S>' to specified type 'Range<S>'}}
    let _: ClosedRange<S> = v..<v // expected-error {{cannot convert value of type 'Range<S>' to specified type 'ClosedRange<S>'}}
    let _: ClosedRange<S> = v... // expected-error {{cannot convert value of type 'PartialRangeFrom<S>' to specified type 'ClosedRange<S>'}}
  }
}

// Check how type inference works with a few commonly used types.
func typeInference_commonTypes() {
  // ---------------------------------------------
  // operator '..<'
  // ---------------------------------------------
  do {
    var range = 1..<10
    expectType(Range<Int>.self, &range)
  }
  do {
    var range = 1..< // expected-error {{'..<' is not a postfix unary operator}}
  }
  do {
    var range = ..<10
    expectType(PartialRangeUpTo<Int>.self, &range)
  }
  do {
    var range = ..<UInt(10)
    expectType(PartialRangeUpTo<UInt>.self, &range)
  }
  do {
    var range = UInt(1)..<10
    expectType(Range<UInt>.self, &range)
  }
  do {
    var range = Int8(1)..<10
    expectType(Range<Int8>.self, &range)
  }
  do {
    var range = UInt8(1)..<10
    expectType(Range<UInt8>.self, &range)
  }
  do {
    var range = 1.0..<10.0
    expectType(Range<Double>.self, &range)
  }
  do {
    var range = ..<10.0
    expectType(PartialRangeUpTo<Double>.self, &range)
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
    var range = ..<"z"
    expectType(PartialRangeUpTo<String>.self, &range)
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
    expectType(ClosedRange<Int>.self, &range)
  }
  do {
    var range = 1...
    expectType(PartialRangeFrom<Int>.self, &range)
  }
  do {
    var range = ...10
    expectType(PartialRangeThrough<Int>.self, &range)
  }
  do {
    var range = UInt(1)...10
    expectType(ClosedRange<UInt>.self, &range)
  }
  do {
    var range = UInt(1)...
    expectType(PartialRangeFrom<UInt>.self, &range)
  }
  do {
    var range = ...UInt(10)
    expectType(PartialRangeThrough<UInt>.self, &range)
  }
  do {
    var range = Int8(1)...10
    expectType(ClosedRange<Int8>.self, &range)
  }
  do {
    var range = UInt8(1)...10
    expectType(ClosedRange<UInt8>.self, &range)
  }
  do {
    var range = UInt8(1)...
    expectType(PartialRangeFrom<UInt8>.self, &range)
  }
  do {
    var range = 1.0...10.0
    expectType(ClosedRange<Double>.self, &range)
  }
  do {
    var range = 1.0...
    expectType(PartialRangeFrom<Double>.self, &range)
  }
  do {
    var range = ...10.0
    expectType(PartialRangeThrough<Double>.self, &range)
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
    var range = "a"...
    expectType(PartialRangeFrom<String>.self, &range)
  }
  do {
    var range = "a"...
    expectType(PartialRangeFrom<String>.self, &range)
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
  do {
    let s = ""
    var range = s.startIndex...
    expectType(PartialRangeFrom<String.Index>.self, &range)
  }
  do {
    let s = ""
    var range = ...s.endIndex
    expectType(PartialRangeThrough<String.Index>.self, &range)
  }
}
