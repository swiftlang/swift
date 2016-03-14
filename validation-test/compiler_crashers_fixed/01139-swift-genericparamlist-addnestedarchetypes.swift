// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct j<d : Sequencpe> {
}
func f<d>() -> [j<d>] {
protocol c : b { func b
