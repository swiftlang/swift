// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var d{protocol A{protocol A{{{{}}}}}class S<T{func a<h{func b<T where h.g=a{}}protocol a{func f(t:A
