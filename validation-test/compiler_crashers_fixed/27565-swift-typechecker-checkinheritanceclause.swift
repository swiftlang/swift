// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct S<T:Sequence{{}class b{struct B<T{class A<T>:b
