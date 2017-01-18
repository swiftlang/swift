// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -emit-sil

// Issue found by https://github.com/rnapier (Rob Napier)

var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in 
}(f, e)
