// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func d<b>() -> [f<b>] {
}
struct f<S: Sequence, e where Optional<e> == S.Iterator.Element
