// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let g{struct Q{struct Q{class A{struct d{class a{{}}struct B{let h:A{{{}}}}}}}}}class S<T{func a<h{func b<T where h.g=a{
