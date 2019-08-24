// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_switch %s | %FileCheck %s

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f1
func f1(_ x : Int32) {
  switch (x) {
  case 1: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:10 : 2
    break
  case 2: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:16 : 3
    fallthrough
  default: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:14 : (3 + 4)
    f1(x - 1)
  } // CHECK: [[@LINE]]:4 -> [[@LINE+3]]:2 : 1

  var y = x
}

enum Algebraic {
  case Type1(Int32, Int32)
  case Type2(Bool)
  case Type3
  case Type4(Bool)
}

func nop() {}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f2
func f2(_ x : Algebraic) -> Int32 {
  switch(x) {
  case let .Type1(y, z): // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:10 : 2
    nop()
  case .Type2(let b): // CHECK: [[@LINE]]:3 -> [[@LINE+2]]:16 : 3
    nop()
    fallthrough
  case .Type3: // CHECK: [[@LINE]]:3 -> [[@LINE+3]]:6 : (3 + 4)
    if (false) { // CHECK: [[@LINE]]:16 -> [[@LINE+2]]:6 : 5
      fallthrough
    }
  case .Type4: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:10 : (5 + 6)
    break
  } // CHECK: [[@LINE]]:4 -> [[@LINE+1]]:11 : 1
  return 0
}

public enum Simple {
  case First, Second
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_switch.f3
func f3(_ x : Simple) -> Int32 {
  switch (x) {
  case .First: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:13 : 2
    return 1
  case .Second: // CHECK: [[@LINE]]:3 -> [[@LINE+1]]:10 : 3
    break
  } // CHECK: [[@LINE]]:4 -> [[@LINE+2]]:11 : 1

  return 0
}

f1(3)
f2(Algebraic.Type1(1, 1))
f2(Algebraic.Type2(false))
f2(Algebraic.Type3)
f3(Simple.Second)
