// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class B<T{struct a{class B<I{struct S<T where g:d{class A:B<T>
