// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
<b
class B<T where h:C{class A<S{protocol c
var d=c
struct c:d
