// RUN: %target-run-simple-swift | %FileCheck %s

// Check that subscripts and functions named subscript can exist side-by-side
struct Foo {
  subscript() -> String {
    return "subscript"
  }
  
  func `subscript`() -> String {
    return "func"
  }
}

let f = Foo()
print(f[]) // CHECK: subscript
print(f.subscript()) // CHECK: func
