// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S{class a{}let e=0 as a{struct B{struct d{struct c<d{{}struct B<T{class A{let a=B{
