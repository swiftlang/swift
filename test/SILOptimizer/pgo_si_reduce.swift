// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_si_reduce -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -profile-use=%t/default.profdata -emit-sorted-sil -Xllvm -sil-print-types -emit-sil -module-name pgo_si_reduce -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -profile-use=%t/default.profdata -O -emit-sorted-sil -Xllvm -sil-print-types -emit-sil -module-name pgo_si_reduce -o - | %FileCheck %s --check-prefix=SIL-OPT

// REQUIRES: profile_runtime
// REQUIRES: executable_test

public func bar(_ x: Int32) -> Int32 {
  if (x == 0) {
    return 42
  }
  if (x == 1) {
    return 6
  }
  if (x == 2) {
    return 9
  }
  if (x % 2 == 0) {
    return 4242
  }
  var ret : Int32 = 0
  for currNum in stride(from: 5, to: x, by: 5) {
    ret += currNum
  }
  return ret
}

// SIL-LABEL: sil @$s13pgo_si_reduce3fooyys5Int32VF : $@convention(thin) (Int32) -> () !function_entry_count(1) {
// SIL-OPT-LABEL: sil @$s13pgo_si_reduce3fooyys5Int32VF : $@convention(thin) (Int32) -> () !function_entry_count(1) {
public func foo(_ x: Int32) {
  // SIL: switch_enum {{.*}} : $Optional<Int32>, case #Optional.some!enumelt: {{.*}} !case_count(100), case #Optional.none!enumelt: {{.*}} !case_count(1)
  // SIL: cond_br {{.*}}, {{.*}}, {{.*}} !true_count(50)
  // SIL: cond_br {{.*}}, {{.*}}, {{.*}} !true_count(1)
  // SIL-OPT: integer_literal $Builtin.Int32, 4242
  // SIL-OPT: integer_literal $Builtin.Int32, 42
  // SIL-OPT: function_ref @$s13pgo_si_reduce3barys5Int32VADF : $@convention(thin) (Int32) -> Int32
  
  var sum : Int32 = 0
  for index in 1...x {
    if (index % 2 == 0) {
      sum += bar(index)
    }
    if (index == 50) {
      sum += bar(index)
    }
    sum += 1
  }
  print(sum)
}
// SIL-LABEL: } // end sil function '$s13pgo_si_reduce3fooyys5Int32VF'
// SIL-OPT-LABEL: } // end sil function '$s13pgo_si_reduce3fooyys5Int32VF'

foo(100)
