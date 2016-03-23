// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
class a<f : b, g : b where f.d == g> {
}
protocol b {
typealias d
}
struct c<h : b> : b {
typealias e = a<c<h>, d>
}
struct c<d, e: b where d.c == e>
