// RUN: %swift -i %s | FileCheck %s

func andc<T:LogicValue>(x:Bool, y:T) -> Bool {
  return x && !y.getLogicValue()
}

struct Truthy : LogicValue {
  func getLogicValue() -> Bool {
    return true
  }
}

struct Falselike : LogicValue {
  func getLogicValue() -> Bool {
    return false
  }
}

println(andc(true, Truthy())) // CHECK: false
println(andc(false, Truthy())) // CHECK: false
println(andc(true, Falselike())) // CHECK: true
println(andc(false, Falselike())) // CHECK: false

func must<T:LogicValue>(x:T) {
  assert(x.getLogicValue())
}
func shant<T:LogicValue>(x:T) {
  assert(!x.getLogicValue())
}

must(Truthy())
shant(Falselike())

println("ok") // CHECK: ok
