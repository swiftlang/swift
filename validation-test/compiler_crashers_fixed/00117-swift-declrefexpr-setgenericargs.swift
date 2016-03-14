// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
b
protocol d : b { func b
func d(e:  = { (g: h, f: h -> h) -> h in
    return f(g)
}
