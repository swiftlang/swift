// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func f{{struct c{enum S{struct B}}}class B<T where a:A{class A{let:{if{A{
