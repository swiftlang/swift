// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let b{class a{struct c{class A{struct c,class A{class B{class A{func a{func b<T:T.c
