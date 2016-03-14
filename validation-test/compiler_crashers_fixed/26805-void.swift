// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct c<T{class A{class b:A{class B{class B<T where g:T{let c{f
