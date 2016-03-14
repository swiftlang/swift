// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<T where g:t{protocol a{func b<d where g.E=C{}}class C{class A{class B<T>:B<T>
