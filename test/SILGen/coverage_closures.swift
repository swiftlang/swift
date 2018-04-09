// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_closures %s | %FileCheck %s

// rdar://39200851: Closure in init method covered twice

class C2 {
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_closures.C2.__allocating_init()
// CHECK-NEXT:  [[@LINE+7]]:10 -> [[@LINE+11]]:4 : 0
// CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// closure #1 () -> () in coverage_closures.C2.init()
// CHECK-NEXT:  [[@LINE+4]]:13 -> [[@LINE+6]]:6 : 0
// CHECK-NEXT: }

  init() {
    let _ = { () in
      print("hello")
    }
  }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_closures.bar
// CHECK-NEXT:  [[@LINE+9]]:35 -> [[@LINE+13]]:2 : 0
// CHECK-NEXT:  [[@LINE+9]]:44 -> [[@LINE+11]]:4 : 1
// CHECK-NEXT:  [[@LINE+10]]:4 -> [[@LINE+11]]:2 : 0
// CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// closure #2 (Swift.Int32) -> Swift.Int32 in coverage_closures.bar
// CHECK-NEXT:  [[@LINE+4]]:13 -> [[@LINE+4]]:42 : 0
// CHECK-NEXT: }

func bar(arr: [(Int32) -> Int32]) {
  for a in [{ (b : Int32) -> Int32 in b }] {
    a(0)
  }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_closures.foo
func foo() {
  let c1 = { (i1 : Int32, i2 : Int32) -> Bool in i1 < i2 }

// CHECK-LABEL: sil_coverage_map {{.*}}// f1 #1 ((Swift.Int32, Swift.Int32) -> Swift.Bool) -> Swift.Bool in coverage_closures.foo()
// CHECK-NEXT: [[@LINE+2]]:55 -> [[@LINE+4]]:4 : 0
// CHECK-NEXT: [[@LINE+2]]:29 -> [[@LINE+2]]:42 : 1
  func f1(_ closure : (Int32, Int32) -> Bool) -> Bool {
    return closure(0, 1) && closure(1, 0)
  }

  f1(c1)

// CHECK-LABEL: sil_coverage_map {{.*}}// closure #2 (Swift.Int32, Swift.Int32) -> Swift.Bool in coverage_closures.foo()
// CHECK-NEXT: [[@LINE+1]]:6 -> [[@LINE+1]]:27 : 0
  f1 { i1, i2 in i1 > i2 }

// CHECK-LABEL: sil_coverage_map {{.*}}// closure #3 (Swift.Int32, Swift.Int32) -> Swift.Bool in coverage_closures.foo()
// CHECK-NEXT: [[@LINE+2]]:6 -> [[@LINE+2]]:48 : 0
// CHECK-NEXT: [[@LINE+1]]:36 -> [[@LINE+1]]:46 : 1
  f1 { left, right in left == 0 || right == 1 }
}
// CHECK-LABEL: sil_coverage_map {{.*}}// closure #1 (Swift.Int32, Swift.Int32) -> Swift.Bool in coverage_closures.foo()
// CHECK-NEXT: [[@LINE-21]]:12 -> [[@LINE-21]]:59 : 0

// SR-2615: Implicit constructor decl has no body, and shouldn't be mapped
struct C1 {
// CHECK-NOT: sil_coverage_map{{.*}}errors
  private var errors = [String]()
}

bar(arr: [{ x in x }])
foo()
let _ = C2()
