// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
// This test does not crash on Linux.
import Foundation
let c: NSObject {
struct Q<T where I.E == c: U : AnyObject, object2: T) {
if true {
}
static let a = c
