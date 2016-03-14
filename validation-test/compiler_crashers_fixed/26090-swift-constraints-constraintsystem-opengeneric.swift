// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let s{class A{class b<T where g:T{class A{var:d=Int
class d<T
