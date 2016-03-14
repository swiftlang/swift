// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct b{class A{{}enum a{enum a{class B{enum B}}}}enum S<T where H:A{struct Q{class B:b{{}}class b
