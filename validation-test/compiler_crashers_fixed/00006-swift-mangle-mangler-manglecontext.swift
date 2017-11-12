// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -typecheck -verify

// Issue found by https://github.com/AlexDenisov (Alexey Denisov)

func i(c: () -> ()) {
}
class a {
    var _ = i() { // expected-error {{property declaration does not bind any variables}}
    }
}
