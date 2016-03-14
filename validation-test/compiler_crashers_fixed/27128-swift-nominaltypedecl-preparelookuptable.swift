// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var A{a{}{class A{struct B<T where B:T{class c{class a<a<let h{var _=a{
