// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol a {
    class func c()
}
class b: a {t c<h : b>  d = h
    typealias e = B
protocol a {
  typealias d
  typealias e = d
  typealias f = d b<h : c, i : c where h.g == i> :)
