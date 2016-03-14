// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
var f = 1
var e: Int -> Int = {
}
let d: Int =  { c, b in
}(f, e)
struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : A>
