// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct A{class B<T where f:A{struct S<T{protocol c<var a{var _=c{
