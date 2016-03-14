// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var b {
struct D : b: [1].R
protocol b {
func b<T, object2: b
class C
