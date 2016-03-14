// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T{class b<T where k:A{struct E{class A{{}}func b{{class b{class B<T>:A
