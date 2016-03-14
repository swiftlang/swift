// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
enum B{class A{class B{class B{struct B<T where k:A{class a{class B{let a=(class A{let s=V
