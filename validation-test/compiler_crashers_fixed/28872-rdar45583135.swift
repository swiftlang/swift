// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -emit-sil

// One day, if we're lucky, this test will start failing by emitting SIL
// instead of diagnosing an error. If you are reading this comment because that
// joyous day has finally arrived, please remove the "not" in the RUN line.
// Until then, we will at least make sure we don't regress and start crashing
// again.

func f() -> Int {
    return { a in
        { [unowned a] b in
            a(b)
        }
    }(1)
}
