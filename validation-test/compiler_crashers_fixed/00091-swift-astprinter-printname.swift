// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
}
func a<g>() -> (g, g -> g) -> g {
    var b: ((g, g -> g) -> g)!
    return b
}
func f<g : d {
    return !(a)
  enum g {
        func g
        var _ = g
