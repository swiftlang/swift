// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -emit-sorted-sil -module-name coverage_toplevel %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK: sil_coverage_map{{.*}}// coverage_toplevel.f1

// CHECK: sil_coverage_map {{.*}} "main"
// CHECK: [[@LINE+1]]:1 -> [[@LINE+1]]:11 : 0
print("a")

// Make sure we don't emit regions for these.
class C {
  var k = 0
  func foo() {}
}
func f1() {}

// CHECK-NEXT: [[@LINE+1]]:1 -> [[@LINE+1]]:18 : 1
var i : Int32 = 0

// CHECK-NEXT: [[@LINE+3]]:1 -> [[@LINE+5]]:2 : 2
// CHECK-NEXT: [[@LINE+2]]:7 -> [[@LINE+2]]:15 : (2 + 3)
// CHECK-NEXT: [[@LINE+1]]:16 -> [[@LINE+3]]:2 : 3
while (i < 10) {
  i += 1
}

// CHECK-NEXT: [[@LINE+3]]:1 -> [[@LINE+3]]:22 : 4
// CHECK-NEXT: [[@LINE+2]]:17 -> [[@LINE+2]]:18 : 5
// CHECK-NEXT: [[@LINE+1]]:21 -> [[@LINE+1]]:22 : (4 - 5)
var i2 = true ? 1 : 0;

// CHECK-NEXT: [[@LINE+3]]:1 -> [[@LINE+5]]:2 : 6
// CHECK-NEXT: [[@LINE+2]]:4 -> [[@LINE+2]]:10 : 6
// CHECK-NEXT: [[@LINE+1]]:11 -> [[@LINE+3]]:2 : 7
if (true) {
  i2 = 2
}

// Crash tests:

if (true) {
  i2 = 3
} else {
  i2 = 4
}

while (i2 > 0) {
  if (true) {
    i2 -= 1
    continue
  } else {
    i2 -= 1
    break
  }
}

switch (1) {
  case 0: fallthrough
  case 1: break
  default: break
}
