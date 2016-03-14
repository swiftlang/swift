// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct D{
struct B<a
}
enum B{
func a<T where f:b
var f:D.B
