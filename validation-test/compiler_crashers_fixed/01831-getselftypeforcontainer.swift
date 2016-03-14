// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol b {
let v: String {
init(.Type) -> (self.Element == ")
typealias A : Array<A>
