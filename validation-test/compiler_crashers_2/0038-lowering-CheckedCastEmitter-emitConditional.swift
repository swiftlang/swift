// RUN: not --crash %target-swift-frontend %s -emit-silgen

let a: () -> Int? = { return nil }
a as? Int
