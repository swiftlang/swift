// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
if { {l{}}
struct B<{}
protocol A
class g: A{
var _ =A
a
var _=B<S:{ }}
class B
