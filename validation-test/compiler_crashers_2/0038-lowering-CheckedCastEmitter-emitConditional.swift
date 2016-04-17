// RUN: not --crash %target-swift-frontend %s -emit-silgen

// REQUIRES: asserts

let a: () -> Int? = { return nil }
a as? Int
