// RUN: %swift -debug-only=switch-silgen -emit-silgen %s >/dev/null 2>&1

// CHECK: Specializing on (,,), at row 0
// CHECK: | Choosing necessary column 2
// CHECK: | Specializing on .A, at row 0
// CHECK: | | Choosing necessary column 1
// CHECK: | | Specializing on .A, at row 0
// CHECK: | | | Choosing necessary column 2
// CHECK: | | | Specializing on .A, at row 0
// CHECK: | Specializing on .B, at row 2
// CHECK: | Specializing on .C, at row 3
// CHECK: | | Choosing necessary column 2
// CHECK: | | Specializing on .A, at row 0
// CHECK: | | | Choosing necessary column 1
// CHECK: | | | Specializing on .A, at row 0

enum Foo { case A, B, C }

var x = Foo.A
var y = x
var z = x

switch (x, y, z) {
case (.A, .A, .A):
case (_,  .A, .A):
case (_,  _,  .B):
case (.A, .A, .C):
case (.A, _,  .C):
case _:
}
