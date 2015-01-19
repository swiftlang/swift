// RUN: %target-swift-frontend %s -emit-silgen

// <rdar://problem/18571392> Passing a var to immediately called Swift closure causes segfault in Xcode 6.1 (6A1042b)

func p<T>(obj : T) {}

var index = 1
_ = {
    p($0)
} (index)
