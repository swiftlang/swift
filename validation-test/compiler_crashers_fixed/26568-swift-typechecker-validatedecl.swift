// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B{struct A{enum S<T{struct A{enum e{case c<func a{c
