// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-silgen
// Test case submitted to project by https://github.com/airspeedswift (airspeedswift)

struct S<T> {
    var a: [T] = []
}
extension S {
    init(other: [T]) {
        a = other
    }
}
