// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -suppress-warnings -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_pound_if %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

func poundIf1() -> Int {
  #if true
    #if true
    return 1
    #else
    return 2
    #endif
  #else
    #if true
    return 3
    #else
    return 4
    #endif
  #endif
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If1SiyF"
// CHECK-NEXT:    [[@LINE-16]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-16]]:3 -> [[@LINE-16]]:11 : skipped
// CHECK-NEXT:    [[@LINE-16]]:5 -> [[@LINE-16]]:13 : skipped
// CHECK-NEXT:    [[@LINE-15]]:5 -> [[@LINE-13]]:11 : skipped
// CHECK-NEXT:    [[@LINE-13]]:3 -> [[@LINE-7]]:9 : skipped
// CHECK-NEXT:  }

func poundIf2(_ x: [Int]) -> [Int] {
  return x
  #if false
    .map { $0 + 1 }
  #endif
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If2ySaySiGACF"
// CHECK-NEXT:    [[@LINE-7]]:36 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-6]]:3 -> [[@LINE-4]]:9 : skipped
// CHECK-NEXT:  }


func poundIf3() -> Any {
#if false
  @objc
#endif
  class C {}
  return C()
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If3ypyF"
// CHECK-NEXT:    [[@LINE-8]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-8]]:1 -> [[@LINE-6]]:7 : skipped
// CHECK-NEXT:  }

func poundIf4(_ x: Bool) -> Int {
  switch x {
  #if true
  case true:
    return 0
  #else
  case false:
    return 0
  #endif
  #if false
  case false:
    return 1
  #endif
  case false:
    return 0
  }
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If4ySiSbF"
// CHECK-NEXT:    [[@LINE-18]]:33 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-18]]:10 -> [[@LINE-18]]:11 : 0
// CHECK-NEXT:    [[@LINE-18]]:3 -> [[@LINE-18]]:11 : skipped
// CHECK-NEXT:    [[@LINE-18]]:3 -> [[@LINE-17]]:13 : 1
// CHECK-NEXT:    [[@LINE-17]]:3 -> [[@LINE-14]]:9 : skipped
// CHECK-NEXT:    [[@LINE-14]]:3 -> [[@LINE-11]]:9 : skipped
// CHECK-NEXT:    [[@LINE-11]]:3 -> [[@LINE-10]]:13 : 2
// CHECK-NEXT:  }

func poundIf5() {
  struct S {
#if false
    var foo: Int
#else
    var foo: Int
#endif
  }
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If5yyF"
// CHECK-NEXT:    [[@LINE-10]]:17 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-9]]:1 -> [[@LINE-7]]:6 : skipped
// CHECK-NEXT:    [[@LINE-6]]:1 -> [[@LINE-6]]:7 : skipped
// CHECK-NEXT:  }

func poundIf6() -> Int {
  #if true
  struct S {
    var foo: Int
  }
  #else
  struct S {
    var foo: Int
  }
  #endif
  return S(foo: 0).foo
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If6SiyF"
// CHECK-NEXT:    [[@LINE-13]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-13]]:3 -> [[@LINE-13]]:11 : skipped
// CHECK-NEXT:    [[@LINE-10]]:3 -> [[@LINE-6]]:9 : skipped
// CHECK-NEXT:  }

func poundIf7() -> Int {
  #if false
  #endif
  return 0
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If7SiyF"
// CHECK-NEXT:    [[@LINE-6]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-6]]:3 -> [[@LINE-5]]:9 : skipped
// CHECK-NEXT:  }

func poundIf8() -> Int {
  #if true
  #endif
  return 0
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If8SiyF"
// CHECK-NEXT:    [[@LINE-6]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-6]]:3 -> [[@LINE-6]]:11 : skipped
// CHECK-NEXT:    [[@LINE-6]]:3 -> [[@LINE-6]]:9 : skipped
// CHECK-NEXT:  }

func poundIf9() -> Int {
  #if true
  #else
  #endif
  return 0
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B3If9SiyF"
// CHECK-NEXT:    [[@LINE-7]]:24 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-7]]:3 -> [[@LINE-7]]:11 : skipped
// CHECK-NEXT:    [[@LINE-7]]:3 -> [[@LINE-6]]:9 : skipped
// CHECK-NEXT:  }

func poundIf10() -> Int {
  #if true
  return 0
  #else
  return 1#endif
}
// CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_pound_if0B4If10SiyF"
// CHECK-NEXT:    [[@LINE-7]]:25 -> [[@LINE-2]]:2 : 0
// CHECK-NEXT:    [[@LINE-7]]:3 -> [[@LINE-7]]:11 : skipped
// CHECK-NEXT:    [[@LINE-6]]:3 -> [[@LINE-5]]:17 : skipped
// CHECK-NEXT:  }
