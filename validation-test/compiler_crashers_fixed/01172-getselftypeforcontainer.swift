// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<f : b, g : b where f.d == g> {
}
protocol b {
typealias d
: C {
}
typealias C = B
func a(b: I
