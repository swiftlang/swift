// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol a {
    class func c()
}
class b: a {t c<h : b>  d = h
    typealias e = B
protocol a {
  typealias d
  typealias e = d
  typealias f = d b<h : c, i : c where h.g == i> :)
