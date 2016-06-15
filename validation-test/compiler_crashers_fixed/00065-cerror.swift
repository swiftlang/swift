// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
q
var m: Int -> Int = {
    n $0
 o: Int = { d, l f
    n l(d)
}(k, m)
protocol j {
  typealias d
  typealias n = d
  typealias l = d}
class g<q : l, m : l p q.g == m> : j {
}
class g<q, m> {
}
protocol l {
    typealias g
