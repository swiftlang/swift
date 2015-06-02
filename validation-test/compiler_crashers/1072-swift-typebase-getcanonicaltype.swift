// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class k: c{  class func i {
}
class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
typealias i
: d {
