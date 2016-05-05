// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

func andc<T : Boolean>(_ x: Bool, _ y: T) -> Bool {
  return x && !y.boolValue
}

struct Truthy : Boolean {
  var boolValue: Bool {
    return true
  }
}

struct Falselike : Boolean {
  var boolValue: Bool {
    return false
  }
}

print(andc(true, Truthy())) // CHECK: false
print(andc(false, Truthy())) // CHECK: false
print(andc(true, Falselike())) // CHECK: true
print(andc(false, Falselike())) // CHECK: false

func must<T : Boolean>(_ x: T) {
  assert(x.boolValue)
}
func shant<T : Boolean>(_ x: T) {
  assert(!x.boolValue)
}

must(Truthy())
shant(Falselike())

print("ok") // CHECK: ok
