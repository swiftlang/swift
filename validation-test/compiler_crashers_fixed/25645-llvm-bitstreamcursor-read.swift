// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol C{
let:C
let h:e
typealias e:d
{}typealias e{
}
class d:e
