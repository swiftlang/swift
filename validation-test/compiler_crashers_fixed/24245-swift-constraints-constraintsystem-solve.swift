// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck

// Issue found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/19924870

func unit<T>(x: T) -> T? {
    return x
}
func f() -> Int? {
    return unit(1) ?? unit(2).map { 1 } ?? nil
}
