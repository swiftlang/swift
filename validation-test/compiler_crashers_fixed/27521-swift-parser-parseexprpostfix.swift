// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T where g:b{class S{func a{struct A{let e=a{}class A{struct c{func g{{
