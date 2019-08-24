// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol f {
}
protocol e : f {
}
protocol i {
}
struct c : i {
}
func i<j : j, d : i j d.c == j> (i: d) {
}
func i<c :{
}
let e = f
protocol e : j { func j
