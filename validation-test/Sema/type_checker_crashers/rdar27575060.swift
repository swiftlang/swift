// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

func f(_ x: Any...) {}

var a = 1
f((a, 2))
