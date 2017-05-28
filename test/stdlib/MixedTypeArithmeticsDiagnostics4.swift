//===--- MixedTypeArithmeticsDiagnostics4.swift ---------------------------===//
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
// RUN: %target-typecheck-verify-swift -swift-version 4


func mixedTypeArithemtics() {
  _ = (42 as Int64) + (0 as Int) // expected-error {{'+' is unavailable}}

  do {
    var x = Int8()
    x += (42 as Int) // expected-error {{'+=' is unavailable}}
  }

  _ = (42 as Int32) - (0 as Int) // expected-error {{'-' is unavailable}}

  do {
    var x = Int16()
    x -= (42 as Int) // expected-error {{'-=' is unavailable}}
  }

  // With Int on both sides should NOT result in warning
  do {
    var x = Int()
    x += (42 as Int)
  }
}

func radar31909031() {
  let x = UInt64()
  let y = UInt64()
  _ = (x - y) < UInt64(42) // should not produce a mixed-type warning
}
