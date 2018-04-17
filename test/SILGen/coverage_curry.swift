// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sil -module-name coverage_curry %s | %FileCheck %s

struct S0 {
  init(a: Int, b: Int) { }
  func f1(a: Int, b: Int) -> Int { return a }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_curry.testS0CurriedInstanceMethods(s0: coverage_curry.S0, a: Swift.Int, b: Swift.Int)
// CHECK-NEXT: [[@LINE+2]]:59 -> [[@LINE+8]]:2 : 0
// CHECK-NEXT: }
func testS0CurriedInstanceMethods(s0: S0, a: Int, b: Int) {
  _ = S0.f1(s0)(a: a, b: a)
  _ = (S0.f1)(s0)(a: a, b: a)
  _ = ((S0.f1))(s0)(a: a, b: a)
  _ = S0.f1(a:b:)(s0)(a, b)
  let f1OneLevel = S0.f1(s0)
}
