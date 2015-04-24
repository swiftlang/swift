// RUN: %target-run-simple-swift | FileCheck %s

func andc<T : BooleanType>(x: Bool, _ y: T) -> Bool {
  return x && !y.boolValue
}

struct Truthy : BooleanType {
  var boolValue: Bool {
    return true
  }
}

struct Falselike : BooleanType {
  var boolValue: Bool {
    return false
  }
}

println(andc(true, Truthy())) // CHECK: false
println(andc(false, Truthy())) // CHECK: false
println(andc(true, Falselike())) // CHECK: true
println(andc(false, Falselike())) // CHECK: false

func must<T : BooleanType>(x: T) {
  assert(x.boolValue)
}
func shant<T : BooleanType>(x: T) {
  assert(!x.boolValue)
}

must(Truthy())
shant(Falselike())

println("ok") // CHECK: ok
