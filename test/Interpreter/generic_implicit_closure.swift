// RUN: %target-run-simple-swift | FileCheck %s

func andc<T : LogicValueType>(x: Bool, y: T) -> Bool {
  return x && !y.getLogicValue()
}

struct Truthy : LogicValueType {
  func getLogicValue() -> Bool {
    return true
  }
}

struct Falselike : LogicValueType {
  func getLogicValue() -> Bool {
    return false
  }
}

println(andc(true, Truthy())) // CHECK: false
println(andc(false, Truthy())) // CHECK: false
println(andc(true, Falselike())) // CHECK: true
println(andc(false, Falselike())) // CHECK: false

func must<T : LogicValueType>(x: T) {
  assert(x.getLogicValue())
}
func shant<T : LogicValueType>(x: T) {
  assert(!x.getLogicValue())
}

must(Truthy())
shant(Falselike())

println("ok") // CHECK: ok
