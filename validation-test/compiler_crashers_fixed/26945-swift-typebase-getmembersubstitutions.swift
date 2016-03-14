// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{enum S<T where g:B{struct d<h{struct A{class B{class B<T:A.B
