// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct s<o : x> {
m m:  [s<o>] {
}
protocol r {
class func s()
}
class m: r {
class func s() { }
}
}
m
protocol s : m { o l.s == m> : l {
}
class o<l, m> {
}
protocol s {
}
protocol s : o { func o
