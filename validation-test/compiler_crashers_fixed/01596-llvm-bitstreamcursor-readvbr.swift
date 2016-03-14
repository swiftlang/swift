// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol A {
protocol d : b {
func b
}
}
struct c() + seq
let c = c(A<f = [self.Gene
