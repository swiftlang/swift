// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
>("
class b<T..startIndex, d.advance()
f: Hashable> {
func d<l : b<d>
