// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol d : AnyObject, b {
func a<Q<b) {
let c("))
typealias e: a
