//===--- MixedTypeArithmeticsDiagnostics.swift ----------------------------===//
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


func mixedTypeArithemtics() {
  _ = (42 as Int64) + (0 as Int) // expected-warning {{'+' is deprecated:}}

  do {
    var x = Int8()
    x += (42 as Int) // expected-warning {{'+=' is deprecated:}}
  }

  _ = (42 as Int32) - (0 as Int) // expected-warning {{'-' is deprecated:}}

  do {
    var x = Int16()
    x -= (42 as Int) // expected-warning {{'-=' is deprecated:}}
  }

  // With Int on both sides should NOT result in warning
  do {
    var x = Int()
    x += (42 as Int)
  }
}
