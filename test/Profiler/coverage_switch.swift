// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_switch %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

// CHECK-LABEL: sil hidden @$s15coverage_switch2f1yys5Int32VF : $@convention(thin) (Int32) -> ()
// CHECK:       increment_profiler_counter 0

// CHECK:       integer_literal $Builtin.Int32, 1
// CHECK:       cmp_eq_Int32
// CHECK:       cond_br {{%[0-9]+}}, [[CASE1:bb[0-9]+]], [[NOTCASE1:bb[0-9]+]]

// CHECK:       [[NOTCASE1]]
// CHECK:       integer_literal $Builtin.Int32, 2
// CHECK:       cmp_eq_Int32
// CHECK:       cond_br {{%[0-9]+}}, [[CASE2:bb[0-9]+]], [[NOTCASE2:bb[0-9]+]]

// CHECK:       [[NOTCASE2]]
// CHECK-NEXT:  increment_profiler_counter 3
// CHECK-NEXT:  br [[DEFAULT:bb[0-9]+]]

// CHECK:       [[CASE2]]
// CHECK-NEXT:  increment_profiler_counter 2
// CHECK-NEXT:  br [[DEFAULT]]

// CHECK:       [[DEFAULT]]
// CHECK:       function_ref @$s15coverage_switch2f1yys5Int32VF : $@convention(thin) (Int32) -> ()

// CHECK:       [[CASE1]]
// CHECK-NEXT:  increment_profiler_counter 1

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f1
func f1(_ x : Int32) { // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+11]]:2 : 0
  switch (x) {         // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:13 : 0
  case 1:              // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:10 : 1
    break
  case 2:              // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:16 : 2
    fallthrough
  default:             // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:14 : (2 + 3)
    f1(x - 1)
  } // CHECK-NEXT: [[@LINE]]:4 -> [[@LINE+3]]:2 : ((1 + 2) + 3)

  var y = x
} // CHECK-NEXT: }

enum Algebraic {
  case Type1(Int32, Int32)
  case Type2(Bool)
  case Type3
  case Type4(Bool)
}

func nop() {}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f2
func f2(_ x : Algebraic) -> Int32 { // CHECK-NEXT: [[@LINE]]:35 -> [[@LINE+16]]:2 : 0
  switch(x) {                       // CHECK-NEXT: [[@LINE]]:9 -> [[@LINE]]:12 : 0
  case let .Type1(y, z):            // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:10 : 1
    nop()
  case .Type2(let b):               // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+2]]:16 : 2
    nop()
    fallthrough
  case .Type3:                      // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+4]]:7 : (2 + 3)
    if (false) {                    // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE]]:15 : (2 + 3)
      fallthrough                   // CHECK-NEXT: [[@LINE-1]]:16 -> [[@LINE+1]]:6 : 4
    }                               // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE+1]]:7 : ((2 + 3) - 4)
    () // Here to make sure this region is non empty ^
  case .Type4:                      // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:10 : (4 + 5)
    break
  } // CHECK-NEXT: [[@LINE]]:4 -> [[@LINE+1]]:11 : (((1 + 2) + 3) + 5)
  return 0
} // CHECK-NEXT: }

public enum Simple {
  case First, Second
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f3
func f3(_ x : Simple) -> Int32 { // CHECK-NEXT: [[@LINE]]:32 -> [[@LINE+9]]:2 : 0
  switch (x) {                   // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:13 : 0
  case .First:                   // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:13 : 1
    return 1
  case .Second:                  // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:10 : 2
    break
  } // CHECK: [[@LINE]]:4 -> [[@LINE+2]]:11 : 2

  return 0
} // CHECK-NEXT: }

f1(3)
f2(Algebraic.Type1(1, 1))
f2(Algebraic.Type2(false))
f2(Algebraic.Type3)
f3(Simple.Second)

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f4
func f4(_ x: Int) throws -> Int { // CHECK-NEXT: [[@LINE]]:33 -> [[@LINE+23]]:2 : 0
  y: do {                         // CHECK-NEXT: [[@LINE]]:9 -> [[@LINE+20]]:4 : 0
    switch x {                    // CHECK-NEXT: [[@LINE]]:12 -> [[@LINE]]:13 : 0
    case 1, 2, 3:                 // CHECK-NEXT: [[@LINE]]:5 -> [[@LINE+4]]:18 : 1
      if .random() {              // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:19 : 1
        return 5                  // CHECK-NEXT: [[@LINE-1]]:20 -> [[@LINE+1]]:8 : 2
      }                           // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE+1]]:18 : (1 - 2)
      fallthrough
    case 4:                       // CHECK-NEXT: [[@LINE]]:5 -> [[@LINE+5]]:9 : ((1 + 3) - 2)
      if .random() {              // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:19 : ((1 + 3) - 2)
        struct E : Error {}       // CHECK-NEXT: [[@LINE-1]]:20 -> [[@LINE+2]]:8 : 4
        throw E()
      }                           // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE+1]]:9 : (((1 + 3) - 2) - 4)
      () // Here to make sure this region is non empty ^
    default:                      // CHECK-NEXT: [[@LINE]]:5 -> [[@LINE+4]]:9 : 5
      if .random() {              // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:19 : 5
        break y                   // CHECK-NEXT: [[@LINE-1]]:20 -> [[@LINE+1]]:8 : 6
      }                           // CHECK-NEXT: [[@LINE]]:8 -> [[@LINE+1]]:9 : (5 - 6)
      () // Here to make sure this region is non empty ^
    }
    f1(0)                         // CHECK-NEXT: [[@LINE-1]]:6 -> [[@LINE+1]]:4 : (((((1 + 3) + 5) - 2) - 4) - 6)
  }
  return 1                        // CHECK-NEXT: [[@LINE-1]]:4 -> [[@LINE]]:11 : ((((1 + 3) + 5) - 2) - 4)
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f5
func f5(_ x: Simple) -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+7]]:2 : 0
  switch x {                  // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:11 : 0
  case .First:                // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:13 : 1
    return 0
  case .Second:               // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:13 : 2
    return 1
  }
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f6
func f6(_ x: Simple) -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+5]]:2 : 0
  switch x {                  // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE]]:11 : 0
  case .First:  return 0      // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE]]:25 : 1
  case .Second: return 1      // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE]]:25 : 2
  }
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f7
func f7() -> Int { // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+10]]:2 : 0
  switch .random() ? Simple.First : .Second {
    // CHECK-NEXT: [[@LINE-1]]:10 -> [[@LINE-1]]:44 : 0
    // CHECK-NEXT: [[@LINE-2]]:22 -> [[@LINE-2]]:34 : 1
    // CHECK-NEXT: [[@LINE-3]]:37 -> [[@LINE-3]]:44 : (0 - 1)
  case .First: // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:13 : 2
    return 0
  case .Second: // CHECK-NEXT: [[@LINE]]:3 -> [[@LINE+1]]:13 : 3
    return 1
  }
} // CHECK-NEXT: }
