// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_if -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_if -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_if -o - | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_if -o - | %FileCheck %s --check-prefix=SIL-OPT
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_if -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: OS=macosx
// REQUIRES: executable_test

// SIL-LABEL: // pgo_if.guess1
// SIL-LABEL: sil @$S6pgo_if6guess11xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(5001) {
// IR-LABEL: define swiftcc i32 @"$S6pgo_if6guess11xs5Int32VAE_tF"
// IR-OPT-LABEL: define swiftcc i32 @"$S6pgo_if6guess11xs5Int32VAE_tF"
public func guess1(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(5000) !false_count(1)
  // SIL-OPT: cond_br {{.*}} !true_count(5000) !false_count(1)

  // IR: br {{.*}}, !prof ![[BWMD:[0-9]+]]
  if (x == 10) {
    return 20
  } else {
    return 30
  }
}

// SIL-LABEL: // pgo_if.guess2
// SIL-LABEL: sil @$S6pgo_if6guess21xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(5001) {
// IR-LABEL: define swiftcc i32 @"$S6pgo_if6guess21xs5Int32VAE_tF"
// IR-OPT-LABEL: define swiftcc i32 @"$S6pgo_if6guess21xs5Int32VAE_tF"
public func guess2(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(5000) !false_count(1)
  // SIL-OPT: cond_br {{.*}} !true_count(5000) !false_count(1)

  // IR: br {{.*}}, !prof ![[BWMD]]
  return (x == 5) ? 10 : 15
}

func main() {
  var guesses : Int32 = 0;

  guesses += guess1(x: 0) + guess2(x: 0)

  for _ in 1...5000 {
    guesses += guess1(x: 10) + guess2(x: 5)
  }
}

main()

// IR: !{!"branch_weights", i32 5001, i32 2}
// IR-OPT: !{!"branch_weights", i32 5001, i32 2}
