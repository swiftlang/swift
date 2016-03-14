// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where g:C{class A<T{var:{enum B{class B<T{class B{class b{let:T{class d:A
