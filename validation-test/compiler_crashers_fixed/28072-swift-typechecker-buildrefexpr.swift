// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let e{if{struct S<T where k:c{class A{class D{let t:Int=e
