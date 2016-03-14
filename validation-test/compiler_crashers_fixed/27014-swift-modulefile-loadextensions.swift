// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct A class A{enum B<T where f:T>:T}class B{{}struct S{enum b{class a
