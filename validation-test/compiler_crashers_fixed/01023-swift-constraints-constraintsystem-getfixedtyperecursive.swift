// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
public var e: String {
return "[1]("
}
struct c {
protocol A {
typealias d: d where T
func a() {
