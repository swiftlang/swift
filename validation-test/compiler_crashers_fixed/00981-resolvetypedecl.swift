// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class B<C> {
init(c: C) {
c(d ())
}
}
class d<j : i, f : i where j.i == f
