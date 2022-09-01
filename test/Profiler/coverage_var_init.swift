// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_var_init %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

struct S {
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1iSivpfi" {{.*}} // variable initialization expression of coverage_var_init.S.i
  // CHECK-NEXT:  [[@LINE+1]]:11 -> [[@LINE+1]]:12 : 0
  var i = 0

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1jSivpfi" {{.*}} // variable initialization expression of coverage_var_init.S.j
  // CHECK-NEXT:  [[@LINE+1]]:11 -> [[@LINE+1]]:16 : 0
  var j = 1 + 2

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1kSiycvpfi" {{.*}} // variable initialization expression of coverage_var_init.S.k
  // CHECK-NEXT:  [[@LINE+3]]:11 -> [[@LINE+3]]:20 : 0
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1kSiycvpfiSiycfU_" {{.*}} // closure #1 () -> Swift.Int in variable initialization expression of coverage_var_init.S.k
  // CHECK-NEXT: [[@LINE+1]]:11 -> [[@LINE+1]]:20 : 0
  var k = { 1 + 2 }

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1lSivpfi" {{.*}} // variable initialization expression of coverage_var_init.S.l
  // CHECK-NEXT:  [[@LINE+1]]:11 -> [[@LINE+1]]:16 : 0
  var l = #line

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1mSaySiGvpfi" {{.*}} // variable initialization expression of coverage_var_init.S.m
  // CHECK-NEXT:  [[@LINE+1]]:11 -> [[@LINE+1]]:20 : 0
  var m = [1, 2, 3]

  // CHECK-LABEL: sil_coverage_map {{.*}} "$s17coverage_var_init1SV1nSSvpfi" {{.*}} // variable initialization expression of coverage_var_init.S.n
  // CHECK-NEXT:  [[@LINE+3]]:11 -> [[@LINE+3]]:33 : 0
  // CHECK-NEXT:  [[@LINE+2]]:26 -> [[@LINE+2]]:27 : 1
  // CHECK-NEXT:  [[@LINE+1]]:30 -> [[@LINE+1]]:31 : (0 - 1)
  var n = "\(.random() ? 1 : 2)"
}

final class VarInit {
  // CHECK: sil_coverage_map {{.*}} "$s17coverage_var_init7VarInitC018initializedWrapperE0SivpfP"
  // CHECK-NEXT: [[@LINE+1]]:4 -> [[@LINE+1]]:42 : 0
  @Wrapper var initializedWrapperInit = 2

  // CHECK: sil_coverage_map {{.*}} "$s17coverage_var_init7VarInitC04lazydE033_49373CB2DFB47C8DC62FA963604688DFLLSSvgSSyXEfU_"
  // CHECK-NEXT: [[@LINE+1]]:42 -> [[@LINE+3]]:4 : 0
  private lazy var lazyVarInit: String = {
    return "lazyVarInit"
  }()

  // CHECK: sil_coverage_map {{.*}} "$s17coverage_var_init7VarInitC05basicdE033_49373CB2DFB47C8DC62FA963604688DFLLSSvpfiSSyXEfU_"
  // CHECK-NEXT: [[@LINE+1]]:38 -> [[@LINE+3]]:4 : 0
  private var basicVarInit: String = {
    return "Hello"
  }()

  // CHECK: sil_coverage_map {{.*}} "$s17coverage_var_init7VarInitC06simpleD033_49373CB2DFB47C8DC62FA963604688DFLLSSvg"
  // CHECK-NEXT: [[@LINE+1]]:33 -> [[@LINE+3]]:4 : 0
  private var simpleVar: String {
    return "Hello"
  }

  func coverageFunction() {
    print(lazyVarInit)
    print(basicVarInit)
    print(simpleVar)
    print(initializedWrapperInit)
  }
}

@propertyWrapper struct Wrapper {
  init(wrappedValue: Int) {}
  var wrappedValue: Int { 1 }
}

VarInit().coverageFunction()
