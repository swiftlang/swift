// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

var f = 1
var e: Int -> Int = {
}
let d: Int =  { c, b in
}(f, e)
struct g<g : e, f: e where f.h = c {
b let g: c
