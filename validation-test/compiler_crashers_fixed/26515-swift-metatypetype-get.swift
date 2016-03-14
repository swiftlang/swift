// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class d<T where T=b{class A{struct B<U:Any}class B<T{protocol c let t=[c
