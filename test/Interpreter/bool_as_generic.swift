// RUN: %swift -i %s | FileCheck %s
// <rdar://problem/13986638> Missing Bool metadata when Bool is used as a generic
// parameter or existential value

operator prefix !! {}
operator infix &&& {}

func [prefix] !!<T:LogicValue>(x:T) -> Bool {
  return x.getLogicValue()
}

func &&&(x:LogicValue, y:[auto_closure] () -> LogicValue) -> Bool {
  return x.getLogicValue() ? y().getLogicValue() : false
}

println(!!true) // CHECK: true
println(!!false) // CHECK: false
println(true &&& true) // CHECK: true
println(true &&& false) // CHECK: false
