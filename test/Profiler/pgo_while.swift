// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_while -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_while -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_while -o - | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_while -o - | %FileCheck %s --check-prefix=SIL-OPT
// need to check Opt support
// %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_while -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// SIL-LABEL: // pgo_while.guessWhile
// SIL-LABEL: sil @$s9pgo_while10guessWhile1xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(42) {
// IR-LABEL: define {{.*}} i32 @"$s9pgo_while10guessWhile1xs5Int32VAE_tF"
// IR-OPT-LABEL: define {{.*}} i32 @"$s9pgo_while10guessWhile1xs5Int32VAE_tF"

public func guessWhile(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(420) !false_count(42)
  // SIL: cond_br {{.*}} !true_count(176400) !false_count(420)
  // SIL-OPT: cond_br {{.*}} !true_count(420) !false_count(42)
  // SIL-OPT: cond_br {{.*}} !true_count(176400) !false_count(420)

  var ret : Int32 = 0
  var currX : Int32 = 0
  while (currX < x) {
    var currInnerX : Int32 = x*42
    while (currInnerX > 0) {
      ret += currInnerX
      currInnerX -= 1
    }
    currX += 1
  }
  return ret
}

func main() {
  var guesses : Int32 = 0;

  for _ in 1...42 {
    guesses += guessWhile(x: 10)
  }
}

main()

// IR: !{!"branch_weights", i32 421, i32 43}
// IR: !{!"branch_weights", i32 176401, i32 421}
// IR-OPT: !{!"branch_weights", i32 421, i32 43}
// IR-OPT: !{!"branch_weights", i32 176401, i32 421}
