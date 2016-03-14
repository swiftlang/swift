// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
extension NSSet {
if c == .b {
}
var f = 1
var e: Int -> Int = {
}
}
import Foundation
extension NSSet {
convenience init(array: Array) {
