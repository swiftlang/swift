// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where I=(){{}class A{struct c<T{{}class B:A{class A{class B{}struct B<T:e.a
