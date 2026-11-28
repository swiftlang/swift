// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_functon_builder %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

@resultBuilder
struct Summer {
  static func buildBlock(_ x: Int...) -> Int {
    return x.reduce(0, +)
  }
  static func buildIf(_ x: Int?) -> Int {
    return x ?? 0
  }
  static func buildEither(first x: Int) -> Int { x }
  static func buildEither(second x: Int) -> Int { x }
}

// CHECK-LABEL: sil_coverage_map {{.*}} "$s24coverage_functon_builder5test0SiyF"
@Summer
func test0() -> Int {
  // CHECK: [[@LINE-1]]:21 -> [[@LINE+3]]:2 : 0
  18
  12
}

// CHECK-LABEL: sil_coverage_map {{.*}} "$s24coverage_functon_builder5test1SiyF"
@Summer
func test1() -> Int {
  // CHECK: [[@LINE-1]]:21 -> [[@LINE+7]]:2 : 0
  18
  12
  if 7 < 23 {
    11
    8
  }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_functon_builder.test2
@Summer
func test2(_ problem: String?, _ ready: Bool) -> Int { // CHECK-NEXT: [[@LINE]]:54 -> [[@LINE+12]]:2 : 0
  if let problem { // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+2]]:4 : 1
    problem.count
  }
  if ready { // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE]]:11 : 0
    1        // CHECK-NEXT: [[@LINE-1]]:12 -> [[@LINE+1]]:4 : 2
  } else if problem == nil { // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE]]:27 : (0 - 2)
    2                       // CHECK-NEXT: [[@LINE-1]]:28 -> [[@LINE+1]]:4 : 3
  }
  if ready { // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE]]:11 : 0
    3        // CHECK-NEXT: [[@LINE-1]]:12 -> [[@LINE+1]]:4 : 4
  }
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_functon_builder.test3
@Summer
func test3(_ outer: Bool, _ inner: Bool) -> Int { // CHECK-NEXT: [[@LINE]]:49 -> [[@LINE+13]]:2 : 0
  if outer { // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE]]:11 : 0
             // CHECK-NEXT: [[@LINE-1]]:12 -> [[@LINE+7]]:4 : 1
    if inner { // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE]]:13 : 1
      1        // CHECK-NEXT: [[@LINE-1]]:14 -> [[@LINE+1]]:6 : 2
    }
    if inner { // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE]]:13 : 1
      2        // CHECK-NEXT: [[@LINE-1]]:14 -> [[@LINE+1]]:6 : 3
    }
  }
  if inner { // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE]]:11 : 0
    3        // CHECK-NEXT: [[@LINE-1]]:12 -> [[@LINE+1]]:4 : 4
  }
} // CHECK-NEXT: }
