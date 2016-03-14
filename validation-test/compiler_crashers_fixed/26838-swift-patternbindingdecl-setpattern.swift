// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T where B:d{struct b{let d=b{struct A{struct B{enum S{let A{class B{{}var _=[a{let a{{struct c{let d
