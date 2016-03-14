// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func b{struct X{enum e{let:{struct X<T:T.E}struct S{enum e{struct S{class B{struct S{{{{}}}enum B
