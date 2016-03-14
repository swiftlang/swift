// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class b{var _=B<b}{class B<T where T:A{class A<T{enum A{class a{enum b{var _=B<T struct B class B
