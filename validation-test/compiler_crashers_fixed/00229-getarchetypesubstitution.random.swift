// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol a {
    class func c()
}
class b: a {t c<h : b>  d = h
    typealias e = B
protocol a {
  typealias d
  typealias e = d
  typealias f = d b<h : c, i : c where h.g == i> :)
