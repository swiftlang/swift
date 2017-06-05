// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_toplevel %s | %FileCheck %s

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+2]]:1
// CHECK:  [[@LINE+1]]:1 -> [[@LINE+1]]:11
print("a")

// CHECK: sil_coverage_map{{.*}}// coverage_toplevel.f1
func f1() {}

var i : Int32 = 0

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+2]]:1
// CHECK:  [[@LINE+1]]:7 -> [[@LINE+1]]:15 : (0 + 1)
while (i < 10) {
  i += 1
}

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+3]]:1
// CHECK-NEXT:  [[@LINE+2]]:17 -> [[@LINE+2]]:18 : 1
// CHECK-NEXT:  [[@LINE+1]]:21 -> [[@LINE+1]]:22 : 0
var i2 = true ? 1 : 0;

// CHECK: sil_coverage_map{{.*}}__tlcd_line:[[@LINE+4]]:1
// CHECK-NEXT:  [[@LINE+3]]:11 -> [[@LINE+5]]:2 : 1
// CHECK-NEXT:  [[@LINE+2]]:1 -> [[@LINE+4]]:2 : 0
// CHECK-NEXT:  [[@LINE+3]]:2 -> [[@LINE+3]]:2 : 0
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
