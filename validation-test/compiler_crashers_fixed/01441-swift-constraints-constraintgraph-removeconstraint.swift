// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func g<l : a {
return """
}
}
class A {
protocol b {
typealias B : B, A
