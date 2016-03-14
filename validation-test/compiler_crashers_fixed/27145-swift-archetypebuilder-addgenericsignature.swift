// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func d{struct S<A{struct A{struct A{let a}}class A<T:T.a
