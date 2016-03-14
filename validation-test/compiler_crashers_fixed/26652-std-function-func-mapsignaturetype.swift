// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T where g:A{class A{class c{struct B<T{class b{enum k{struct D{struct g{}}}}let e=B
