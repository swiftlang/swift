// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
extension NSSet {
func g<T where T.E == F>(f: B<T>) {
}
}
func b(c) -> <d>(() -> d) {
}
import Foundation
extension NSSet {
convenience init(array: Array) {
