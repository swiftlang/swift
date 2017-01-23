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


// Mixed-type arithmetic operators on Strideable were deprecated in Swift 3.1

func mixedTypeArithemtics() {
  _ = (42 as Int64) + (0 as Int) // expected-error {{binary operator '+' cannot be applied to operands of type 'Int64' and 'Int'}} expected-note {{overloads for '+' exist}}

  do {
    var x = Int8()
    x += (42 as Int) // expected-error {{binary operator '+=' cannot be applied to operands of type 'Int8' and 'Int'}} expected-note {{overloads for '+=' exist}}
  }

  _ = (42 as Int32) - (0 as Int) // expected-error {{binary operator '-' cannot be applied to operands of type 'Int32' and 'Int'}} expected-note {{overloads for '-' exist}}

  do {
    var x = Int16()
    x -= (42 as Int) // expected-error {{binary operator '-=' cannot be applied to operands of type 'Int16' and 'Int'}} expected-note {{overloads for '-=' exist}}
  }

  // With Int on both sides should NOT result in warning
  do {
    var x = Int()
    x += (42 as Int)
  }
}
