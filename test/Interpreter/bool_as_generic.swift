// RUN: %target-run-simple-swift | FileCheck %s
// <rdar://problem/13986638> Missing Bool metadata when Bool is used as a generic
// parameter or existential value

operator prefix !! {}
operator infix &&& {}

@prefix func !!<T : LogicValueType>(x: T) -> Bool {
  return x.getLogicValue()
}

func &&&(x: LogicValueType, y: @auto_closure () -> LogicValueType) -> Bool {
  return x.getLogicValue() ? y().getLogicValue() : false
}

println(!!true) // CHECK: true
println(!!false) // CHECK: false
println(true &&& true) // CHECK: true
println(true &&& false) // CHECK: false
