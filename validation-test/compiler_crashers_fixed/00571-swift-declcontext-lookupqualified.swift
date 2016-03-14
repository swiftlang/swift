// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol d {
func i<l : d where l.f == c> (n: l) {
}
struct c<d: Sequence, b where Optional<b> == d.Iterator.Element>
