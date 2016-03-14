// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where g:a{class A{class B:A{var f{{struct S{struct d{struct c{protocol a
