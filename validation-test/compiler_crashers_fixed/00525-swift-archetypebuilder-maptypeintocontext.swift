// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
func d<v: p>() -> [v] {
func u() {
var d = [[Int]]()
for h in 0..<1 {
d[h][h] = 1
}
}
m p {
}
class w<d> {
init <p: p k p.r == d>(j: p.r) {
}
}
class p {
class func u() -> String {
u
