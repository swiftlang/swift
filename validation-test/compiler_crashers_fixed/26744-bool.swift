// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class C<T where a=V{class A<T{class B:b class b{class h{A}}}
