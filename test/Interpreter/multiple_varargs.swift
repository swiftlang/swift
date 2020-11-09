// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

func vf(x: Int..., y: Int...) {
	print(x, y)
}

vf(x: 1, 2, 3, y: 4, 5, 6)
// CHECK: [1, 2, 3] [4, 5, 6]
vf(y: 1, 2)
// CHECK: [] [1, 2]
vf(x: 3, 4)
// CHECK: [3, 4] []

func vf2(_ x: Int..., y: Int, _ z: Int...) {
  print(x, y, z)
}

vf2(1, 2, 3, y: 4, 5, 6, 7)
// CHECK: [1, 2, 3] 4 [5, 6, 7]
vf2(y: 4, 5, 6, 7)
// CHECK: [] 4 [5, 6, 7]
vf2(1, 2, 3, y: 4)
// CHECK: [1, 2, 3] 4 []
vf2(y: 4)
// CHECK: [] 4 []

func vf3(_ x: Int..., y: Int = 42, _ z: Int...) {
  print(x, y, z)
}

vf3(1, 2, 3, y: 4, 5, 6, 7)
// CHECK: [1, 2, 3] 4 [5, 6, 7]
vf3(y: 4, 5, 6, 7)
// CHECK: [] 4 [5, 6, 7]
vf3(1, 2, 3, y: 4)
// CHECK: [1, 2, 3] 4 []
vf3(y: 4)
// CHECK: [] 4 []

vf3()
// CHECK: [] 42 []
vf3(1, 2, 3)
// CHECK: [1, 2, 3] 42 []

func foo(a: Int..., b: Int, c: Int..., d: Int) {
  print("one")
}

func foo(a: [Int], b: Int, c: [Int], d: Int) {
  print("two")
}

func foo(a: Int..., b: Int, c: [Int], d: Int) {
  print("three")
}

foo(a: 1, 2, 3, b: 4, c: 5, 6, 7, d: 8)
// CHECK: one
foo(a: [1, 2, 3], b: 4, c: [5, 6, 7], d: 8)
// CHECK: two
foo(a: 1, 2, 3, b: 4, c: [5, 6, 7], d: 8)
// CHECK: three

struct Baz {
  init(a: Int..., b: Int...) {
    print(a, b)
  }

  init(_ a: Int..., b: String, _ c: Int...) {
    print(a, b, c)
  }

  subscript(a: Int..., b b: Int...) -> [Int] { a + b }
}

let baz1 = Baz(a: 1, 2, 3, b: 4, 5, 6)
// CHECK: [1, 2, 3] [4, 5, 6]

let baz2 = Baz(1, 2, 3, b: "hello, world!", 3, 2, 1)
// CHECK: [1, 2, 3] hello, world! [3, 2, 1]

print(baz1[1, 2, b: 3, 4])
// CHECK: [1, 2, 3, 4]
