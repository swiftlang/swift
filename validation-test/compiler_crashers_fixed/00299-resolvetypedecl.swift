// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<b : b, d : b where b.d == d> {
}
protocol b {
    typealias d
    typealias e = a<c<h>, d>
}
struct c<d, e: b where d.c == e
