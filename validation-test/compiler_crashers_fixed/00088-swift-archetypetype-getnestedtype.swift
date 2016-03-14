// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct l<e : Sequence> {
    l g: e
}
func h<e>() -> [l<e>] {
    f []
}
func i(e: g) -> <j>(() -> j) -> k
