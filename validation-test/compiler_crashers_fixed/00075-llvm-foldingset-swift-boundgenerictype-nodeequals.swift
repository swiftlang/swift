// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class k<g>: d {
    var f: g
    init(f: g) {
        self.f = f
        l. d {
    typealias i = l
    typealias j = j<i<l>, i>
}
class j {
    typealias d = d
