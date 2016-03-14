// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class A<T where g:e
{
class B<T{struct d
class A<T where g:d>:d
