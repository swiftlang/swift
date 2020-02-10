// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_functon_builder %s | %FileCheck %s

@_functionBuilder
struct Summer {
  static func buildBlock(_ x: Int...) -> Int {
    return x.reduce(0, +)
  }
  static func buildIf(_ x: Int?) -> Int {
    return x ?? 0
  }
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
