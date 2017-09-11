// RUN: %target-run-simple-swift
// REQUIRES: executable_test

class C {
  var a: Int = 0
  var b: Int = 0
}

@inline(never)
func foo<T>(_ item: T, update: (inout T) throws -> Void) rethrows -> T {
  var this = item
  try update(&this)
  return this
}

// Test single statement closure because it's type-checked
// together with the call to `foo`

let rdar34333874_1 = foo(C()) {
  $0.a = 42
}

// The multi-statement closure which is type-checked
// separately from call to `foo`

let rdar34333874_2 = foo(C()) {
  $0.a = 42
  $0.b = 0
}

print(rdar34333874_1)
print(rdar34333874_2)

// Example which avoids mutating fields of the class

@inline(never)
func bar(_ o : C) {
  let _ = foo(o) { (item) in
  }
}

bar(C())
