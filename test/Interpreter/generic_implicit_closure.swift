// RUN: %target-run-simple-swift | FileCheck %s

func andc<T : BooleanType>(x: Bool, y: T) -> Bool {
  return x && !y.getLogicValue()
}

struct Truthy : BooleanType {
  func getLogicValue() -> Bool {
    return true
  }
}

struct Falselike : BooleanType {
  func getLogicValue() -> Bool {
    return false
  }
}

println(andc(true, Truthy())) // CHECK: false
println(andc(false, Truthy())) // CHECK: false
println(andc(true, Falselike())) // CHECK: true
println(andc(false, Falselike())) // CHECK: false

func must<T : BooleanType>(x: T) {
  assert(x.getLogicValue())
}
func shant<T : BooleanType>(x: T) {
  assert(!x.getLogicValue())
}

must(Truthy())
shant(Falselike())

println("ok") // CHECK: ok
