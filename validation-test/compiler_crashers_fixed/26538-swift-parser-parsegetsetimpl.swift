// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class C<T where S:A{struct S<S{class A{class a{func b{
let t{}}}{}}class B
let s=B
