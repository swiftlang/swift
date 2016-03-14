// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c{protocol A:a{}protocol a{{}}class P{enum S{struct S<B{struct B{struct X<T:T.a
