// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class d<T, f: d where T: P {
let t: NSObject {
var e: T
