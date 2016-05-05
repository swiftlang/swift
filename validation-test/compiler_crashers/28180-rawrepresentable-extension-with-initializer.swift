// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not --crash %target-swift-frontend %s -parse

// ASAN Output: stack-overflow on address 0x7fff31bf3ff8 (pc 0x0000022f8f44 bp 0x7fff31bf49d0 sp 0x7fff31bf4000 T0)

extension RawRepresentable {
    init?(rawValue optionalRawValue: RawValue?) {
        guard let rawValue = optionalRawValue, value = Self(rawValue: rawValue) else { return nil }
        self = value
    }
}

enum E: Int {
    case A = 0, B
}

let v: E = .A
