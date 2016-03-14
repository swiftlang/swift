// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{struct a{class B:var f=B{}}enum b{struct S{struct c<T where h:a{struct S<A{struct Q{struct c,let a=c
