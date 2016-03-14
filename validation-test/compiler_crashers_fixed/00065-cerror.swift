// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
q
var m: Int -> Int = {
    n $0
 o: Int = { d, l f
    n l(d)
}(k, m)
protocol j {
  typealias d
  typealias n = d
  typealias l = d}
class g<q : l, m : l p q.g == m> : j {
}
class g<q, m> {
}
protocol l {
    typealias g
