// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let a{class A{class B{class A{class A{func f<d}}}}enum a{{}enum S<T where T:a{class B
