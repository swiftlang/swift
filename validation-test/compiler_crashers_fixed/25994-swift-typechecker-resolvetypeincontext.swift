// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c{struct Q{struct A<T where T:A{class a{class d<T where T:a
