// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
