// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var a{{}""struct S<T where T:c{struct S<T{struct B{func c{B
