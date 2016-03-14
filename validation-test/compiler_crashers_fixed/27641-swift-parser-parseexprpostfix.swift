// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct T{struct S<T where h:e{enum b{enum a{struct A{let s=A{}struct c{let:{{t
