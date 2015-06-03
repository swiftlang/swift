// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

protocol Showable {
  func show()
}

extension Int : Showable {
  func show() {
    print(self)
  }
}

/*FIXME crashes irgen

func show_slice<T : Showable>(xs: [T]) {
  for x in xs {
    x.show()
  }
}
*/

var s = [ 6, 0, 2, 2, 1, 4]
for x in s {
  x.show()
}
// CHECK: 6
// CHECK: 0
// CHECK: 2
// CHECK: 2
// CHECK: 1
// CHECK: 4

for x in [9, 8, 1, 0, 5] {
  x.show()
}
// CHECK: 9
// CHECK: 8
// CHECK: 1
// CHECK: 0
// CHECK: 5

func slice_from_varargs(xs: Int...) {
  for x in xs {
    x.show()
  }
}

slice_from_varargs(1,6,1,8)
// CHECK: 1
// CHECK: 6
// CHECK: 1
// CHECK: 8

