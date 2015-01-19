// RUN: %target-run-simple-swift | FileCheck %s

var n = [2, 3, 5, 7, 11]
var s = ["two", "three", "five", "seven", "eleven", "thirteen"]

var i = 0
var prefix = ""
for p in zip(n, s) {
    print("\(prefix)\(p.0) => \(p.1)")
    ++i
    prefix = ", "
}
println(" (\(i) items)")
// CHECK: 2 => two, 3 => three, 5 => five, 7 => seven, 11 => eleven (5 items)

i = 0
prefix = ""
for p in zip(s, n) {
    print("\(prefix)\(p.0) => \(p.1)")
    ++i
    prefix = ", "
}
println(" (\(i) items)")
// CHECK: two => 2, three => 3, five => 5, seven => 7, eleven => 11 (5 items)

println("done.")

