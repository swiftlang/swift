// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where B:a{struct A{class B{class A<f{func a<P{a<S}}}struct S<T
