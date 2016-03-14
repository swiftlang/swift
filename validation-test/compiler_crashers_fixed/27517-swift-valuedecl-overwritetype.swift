// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct S<T where h:a{class A{class B:A{var a{{struct Q{struct Q{{}class A{class B{struct S{let a{a
