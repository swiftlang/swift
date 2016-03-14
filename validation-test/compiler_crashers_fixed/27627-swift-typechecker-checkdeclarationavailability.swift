// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct Q<T where h:A{class B<T{struct B{let a=B class A{{}}struct B<class B<T where T:A
