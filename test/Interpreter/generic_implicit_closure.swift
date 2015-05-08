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

print(andc(true, Truthy())) // CHECK: false
print(andc(false, Truthy())) // CHECK: false
print(andc(true, Falselike())) // CHECK: true
print(andc(false, Falselike())) // CHECK: false

func must<T : BooleanType>(x: T) {
  assert(x.boolValue)
}
func shant<T : BooleanType>(x: T) {
  assert(!x.boolValue)
}

must(Truthy())
shant(Falselike())

print("ok") // CHECK: ok
