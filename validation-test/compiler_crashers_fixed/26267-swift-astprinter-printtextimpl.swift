// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where g:A{class A{struct d{class B{struct c{class B{class d<l{class a:a
