// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var d{struct S<T{func a<h{func b<T where h.g=a{}}struct c{{{}}struct Q{struct Q{struct c{protocol P
