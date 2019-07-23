// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_si_inlinelarge -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_si_inlinelarge -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_si_inlinelarge -o - | %FileCheck %s --check-prefix=SIL-OPT

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

public func bar(_ x: Int64) -> Int64 {
  if (x == 0) {
    return 42
  }
  if (x == 1) {
    return 6
  }
  if (x == 2) {
    return 9
  }
  if (x == 3) {
    return 93
  }
  if (x == 4) {
    return 94
  }
  if (x == 5) {
    return 95
  }
  if (x == 6) {
    return 96
  }
  if (x == 7) {
    return 97
  }
  if (x == 8) {
    return 98
  }
  if (x == 9) {
    return 99
  }
  if (x == 10) {
    return 910
  }
  if (x == 11) {
    return 911
  }
  if (x == 11) {
    return 911
  }
  if (x == 11) {
    return 911
  }
  if (x == 12) {
    return 912
  }
  if (x == 13) {
    return 913
  }
  if (x == 14) {
    return 914
  }
  if (x == 15) {
    return 916
  }
  if (x == 17) {
    return 917
  }
  if (x == 18) {
    return 918
  }
  if (x == 19) {
    return 919
  }
  if (x % 2 == 0) {
    return 4242
  }
  var ret : Int64 = 0
  for currNum in stride(from: 5, to: x, by: 5) {
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
    print("in bar stride")
    ret += currNum
    print(ret)
  }
  return ret
}

// SIL-LABEL: sil @$s18pgo_si_inlinelarge3fooyys5Int64VF : $@convention(thin) (Int64) -> () !function_entry_count(1) {
// SIL-OPT-LABEL: sil @$s18pgo_si_inlinelarge3fooyys5Int64VF : $@convention(thin) (Int64) -> () !function_entry_count(1) {
public func foo(_ x: Int64) {
  // SIL: switch_enum {{.*}} : $Optional<Int64>, case #Optional.some!enumelt.1: {{.*}} !case_count(100), case #Optional.none!enumelt: {{.*}} !case_count(1)
  // SIL: cond_br {{.*}}, {{.*}}, {{.*}} !true_count(50)
  // SIL: cond_br {{.*}}, {{.*}}, {{.*}} !true_count(1)
  // SIL-OPT: integer_literal $Builtin.Int64, 93
  // SIL-OPT: integer_literal $Builtin.Int64, 42
  // SIL-OPT: function_ref @$s18pgo_si_inlinelarge3barys5Int64VADF : $@convention(thin) (Int64) -> Int64

  var sum : Int64 = 0
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
// SIL-LABEL: } // end sil function '$s18pgo_si_inlinelarge3fooyys5Int64VF'
// SIL-OPT-LABEL: } // end sil function '$s18pgo_si_inlinelarge3fooyys5Int64VF'

foo(100)
