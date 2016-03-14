// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let b{struct S<T{{{}}class A{class B:A{struct S{enum S{func a
