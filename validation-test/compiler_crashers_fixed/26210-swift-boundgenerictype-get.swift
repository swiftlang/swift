// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<h{class d<T where I:a{func b{enum S<T where I:A>:a
