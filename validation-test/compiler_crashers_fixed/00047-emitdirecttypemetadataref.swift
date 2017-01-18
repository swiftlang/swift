// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-ir

// Issue found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/18248167
// https://twitter.com/rob_rix/status/507976289564000258

enum S<T> {
    case C(T, () -> ())
}
