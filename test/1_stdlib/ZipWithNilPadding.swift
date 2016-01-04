// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// FIXME(prext): remove this file when protocol extensions land.

var n = [2, 3, 5, 7, 11]
var s = ["two", "three", "five", "seven", "eleven", "thirteen"]

func stringValue<T>(valueOrNil: T?, defaultValue: String = "nil") -> String {
    if let value = valueOrNil {
        return "\(value)"
    } else {
        return defaultValue
    }
}

var i = 0
var prefix = ""
for p in zipWithNilPadding(n, s) {
    print("\(prefix)\(stringValue(p.0)) => \(stringValue(p.1))", terminator: "")
    i += 1
    prefix = ", "
}
print(" (\(i) items)")
// CHECK: 2 => two, 3 => three, 5 => five, 7 => seven, 11 => eleven, nil => thirteen (6 items)

i = 0
prefix = ""
for p in zipWithNilPadding(s, n) {
    print("\(prefix)\(stringValue(p.0)) => \(stringValue(p.1))", terminator: "")
    i += 1
    prefix = ", "
}
print(" (\(i) items)")
// CHECK: two => 2, three => 3, five => 5, seven => 7, eleven => 11, thirteen => nil (6 items)

print("done.")

