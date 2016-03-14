// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol P{
protocol e class a
typealias e:A
class A{
}
typealias e:a
