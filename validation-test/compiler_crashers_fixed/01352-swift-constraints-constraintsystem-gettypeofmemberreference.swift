// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a : C
var b(b(c: a {
}
protocol A : A {
typealias A
