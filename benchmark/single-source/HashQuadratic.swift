//===--- HashQuadratic.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

let size = 3_000_000

@inline(never)
public func run_HashQuadratic(_ N: Int) {
    for _ in 1...N {
        var dict1: [Int: Int] = [:]
        for i in 0..<size {
            dict1[i] = i * 2
        }

        var dict2: [Int: Int] = [:]
        for (k, v) in dict1 {
            dict2[k] = v
        }
    
        CheckResults(dict2[size/2] == dict2[size/2],
            "Incorrect results in HashQuadratic")
    }
}
