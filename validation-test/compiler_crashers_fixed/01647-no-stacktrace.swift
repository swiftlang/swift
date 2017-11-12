// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-silgen

// Issue found by https://github.com/radex
// rdar://18851497

struct A {
    private let b: [Int] = []
    func a() {
    }
}
typealias B = (C, C)
enum C {
    case D
    case E
}
func c() {
    let d: (B, A)? = nil
    let (e, f) = d!
    f.a()
}
