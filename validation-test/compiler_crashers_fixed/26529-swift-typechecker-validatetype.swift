// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c<h{class A:b{struct S<T where g:A{class B<T>:A{var f=a
