// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class a<T where T: A : b.Type) -> T : (n: b {
"""
enum A : Collection where S() -> {
}
typealias e = b(a)
