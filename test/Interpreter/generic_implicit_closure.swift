// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

protocol MyBoolean {
  var boolValue: Bool { get }
}

func andc<T : MyBoolean>(_ x: Bool, _ y: T) -> Bool {
  return x && !y.boolValue
}

struct Truthy : MyBoolean {
  var boolValue: Bool {
    return true
  }
}

struct Falselike : MyBoolean {
  var boolValue: Bool {
    return false
  }
}

print(andc(true, Truthy())) // CHECK: false
print(andc(false, Truthy())) // CHECK: false
print(andc(true, Falselike())) // CHECK: true
print(andc(false, Falselike())) // CHECK: false

func must<T : MyBoolean>(_ x: T) {
  assert(x.boolValue)
}
func shant<T : MyBoolean>(_ x: T) {
  assert(!x.boolValue)
}

must(Truthy())
shant(Falselike())

print("ok") // CHECK: ok
