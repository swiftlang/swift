// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir -verify %s

// rdar://problem/49818962
func foo() -> some Collection {
  return [12,3]
}

let a = foo()

print(a)
