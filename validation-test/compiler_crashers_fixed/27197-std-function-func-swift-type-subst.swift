// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class C<a,A{struct B{{}enum c{class a{func b<b{struct B<T{class c:a
