// RUN: not --crash %target-swift-frontend %s -emit-silgen
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.reddit.com/r/swift/comments/2gx7m4/psa_nasty_bug_in_the_swift_compiler/cko42gk
// rdar://problem/18734297

func a() -> Int {
    var b = 0
    struct c {
        var d = b
    }
    var d = c()
    return d.d
}
