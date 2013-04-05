// RUN: %swift -i -sil-irgen %s | FileCheck %s

protocol Showable {
  func show()
}

extension Int : Showable {
  func show() {
    println(this)
  }
}

/*FIXME crashes irgen

func show_slice<T:Showable>(xs:T[]) {
  for x in xs {
    x.show()
  }
}
*/

var s = new Int[6]
s[0] = 6
s[1] = 0
s[2] = 2
s[3] = 2
s[4] = 1
s[5] = 4
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

func slice_from_varargs(xs:Int...) {
  for x in xs {
    x.show()
  }
}

slice_from_varargs(1,6,1,8)
// CHECK: 1
// CHECK: 6
// CHECK: 1
// CHECK: 8

