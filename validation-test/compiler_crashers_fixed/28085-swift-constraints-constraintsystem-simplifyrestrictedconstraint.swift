// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct B<T{func a
class d{let e=B}struct B<T:T.b
var f=a{
