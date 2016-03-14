// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where I:b{struct B{let a{let:d=a<{}}class d<T
