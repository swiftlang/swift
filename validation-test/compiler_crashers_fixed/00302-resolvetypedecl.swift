// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct d<b : e, d: e where d.e == b }
func c() {
     for c in 0..<d
