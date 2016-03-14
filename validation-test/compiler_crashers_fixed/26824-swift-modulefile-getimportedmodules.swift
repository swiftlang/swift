// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where g:a{func a{struct S<T where h:p}class B<b{class A{var b{class c:A
