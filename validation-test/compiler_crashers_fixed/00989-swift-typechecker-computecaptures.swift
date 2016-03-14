// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var f = 1
var e: Int -> Int = {
}
let d: Int =  { c, b in
}(f, e)
struct g<g : e, f: e where f.h = c {
b let g: c
