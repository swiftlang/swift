// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{class b<T{struct B<T where g:A{struct S{struct A}struct S<I{class d:b
