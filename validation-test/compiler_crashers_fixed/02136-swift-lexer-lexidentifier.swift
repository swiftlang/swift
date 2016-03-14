// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a {
typealias e == nil
typealias b : b, T where T, range.E
var e: B
class B : d
