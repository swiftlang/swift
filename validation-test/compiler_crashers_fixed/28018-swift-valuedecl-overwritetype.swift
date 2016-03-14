// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
let:{class c{class S{struct Q{class c{let f=1
var f=}}}}struct X<T:T.a
