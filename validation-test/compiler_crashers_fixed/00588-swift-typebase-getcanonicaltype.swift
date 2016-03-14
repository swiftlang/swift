// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol P {
let c: a {
}
protocol a {
extension NSSet {
}
var e: NSObject {
}
typealias e : e {
