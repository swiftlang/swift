// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var:{struct B<T where f:T{class A{class a{class A{class B:a=class B{enum S{{{}}class A{class A{class A{struct B{{}class c{let:a{{
