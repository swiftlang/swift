// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{class a{struct S<T where T:T{struct T}}protocol a{{{}}class b{let a}}
