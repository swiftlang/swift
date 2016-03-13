// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
dynamic
func j<h> -> h {
e { -> h
}
b k
class f {
class func g() -> String {
struct j {
l let d: String = {
i self.g()
}()
__LINE__
protocol f {
}
protocol j : f {
}
protocol e : f {
}
protocol i {
b c = f
}
struct c : i {
b c = j
}
func i<j : j, d : i j d
