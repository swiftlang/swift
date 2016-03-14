// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func a
extension NSSet {
}
func a: Collection where T -> ()
protocol P {
func b<T : b) -> {
