// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct s<o : x> {
m m:  [s<o>] {
}
protocol r {
class func s()
}
class m: r {
class func s() { }
}
}
m
protocol s : m { o l.s == m> : l {
}
class o<l, m> {
}
protocol s {
}
protocol s : o { func o
