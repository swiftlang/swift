// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not --crash %target-swift-frontend %s -parse

// Issue found by https://github.com/austinzheng (Austin Zheng)

enum A<T> {
    case Just(T)
    case Error
}

func foo() -> A<(String, String?)> {
    return A.Just("abc", "def")
}
