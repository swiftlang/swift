// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
{
b: NSObject {
static let b {
(()
}
}
}
func b<Int) {
protocol e : a {
func a
