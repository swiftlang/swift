// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{class C<T where b=a{class a{class A:a{class C<T where g:A{struct a
