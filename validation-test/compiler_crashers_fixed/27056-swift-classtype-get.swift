// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T{{}class a:enum S<T where I:a{struct D{var b={enum SS<T where k:A>:a
