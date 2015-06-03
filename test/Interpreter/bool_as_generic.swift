// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// <rdar://problem/13986638> Missing Bool metadata when Bool is used as a generic
// parameter or existential value

prefix operator !! {}
infix operator &&& {}

prefix func !!<T : BooleanType>(x: T) -> Bool {
  return x.boolValue
}

func &&&(x: BooleanType, @autoclosure y: () -> BooleanType) -> Bool {
  return x.boolValue ? y().boolValue : false
}

print(!!true) // CHECK: true
print(!!false) // CHECK: false
print(true &&& true) // CHECK: true
print(true &&& false) // CHECK: false
