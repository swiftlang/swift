// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

// CHECK: define void @main()
// CHECK: %x = alloca %_TSs5int64, align 8
// CHECK: %x2 = alloca %_TSs5int64, align 8

if (true) {
  var x = 3
  print(x)
} else {
  var x = 3
  print(x)
}
