// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct d<T where g:b{struct S{class b{protocol A{struct Q<a{{}}}class B{class d{let b:T:func f:A
