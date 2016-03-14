// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a{struct S<T{class A{enum b{let a={let:{{struct A<f:T.a
