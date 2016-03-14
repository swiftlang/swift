// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol B {
protocol b : a {
func a() ->() -> String {
}
}
}
static let f = B
