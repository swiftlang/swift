// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
