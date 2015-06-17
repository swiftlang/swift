//===--- Unavailable.swift - Tests for availability diagnostics -----------===//
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
//
//  When we rename or reformulate standard library components, this
//  gives us a place to verify that we've made them unavailable and
//  that the diagnostics make sense.
//
//===----------------------------------------------------------------------===//
// RUN: %target-parse-verify-swift


join([1], [[2], [3]])  // expected-error{{'join' is unavailable: call the 'join' method on the first argument}}
