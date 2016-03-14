// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class a{enum B<T where I:A{class c{class B{let a{class A{let f=a<T:{}}}}var f:a
