// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var f1: Int -> Int = {
    return $0
 crashes: Int = { x, f in
}(x1, f1)
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
