// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let:{if true{struct b{class a{struct S{enum a{struct S<T:T.c
