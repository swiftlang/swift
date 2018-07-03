// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -typecheck -verify

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)
// rdar://18175202

func d<b: Sequence, e>(c : b) -> e? where Optional<e> == b.Iterator.Element {
  for mx : e? in c { // expected-warning {{immutable value 'mx' was never used}}
  }
}
