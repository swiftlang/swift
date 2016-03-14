// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c{struct c{struct b{struct d<T where h:A{struct S<T{class A{let:{class B:A
