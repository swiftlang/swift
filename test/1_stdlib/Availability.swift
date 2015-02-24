//===--- Availability.swift - Tests for availability diagnostics ----------===//
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
typealias A = Slice<Int> // expected-error {{'Slice' has been renamed to ArraySlice}}{{15-20=ArraySlice}}
