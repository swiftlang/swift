// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -typecheck -verify

// Issue found by https://github.com/austinzheng (Austin Zheng)

enum A<T> {
    case Just(T)
    case Error
}

func foo() -> A<(String, String?)> {
    _ = A<(String, String?)>.Just("abc", "def") // expected-error{{extra argument in call}}
    _ = A.Just("abc", "def") // no error?
    return A.Just("abc", "def") // expected-error{{extra argument in call}}
}
