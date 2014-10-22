// RUN: %swift %s -emit-silgen

// <rdar://problem/18571392> Passing a var to immediately called Swift closure causes segfault in Xcode 6.1 (6A1042b)

var index = 1
var b = {
    println($0)
} (index)
