// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_guard -o %t/main
// RUN: env LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_guard -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_guard -o - | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_guard -o - | %FileCheck %s --check-prefix=SIL-OPT
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_guard -o - | %FileCheck %s --check-prefix=IR-OPT

// REQUIRES: profile_runtime
// REQUIRES: OS=macosx

// SIL-LABEL: // pgo_guard.guess1
// SIL-LABEL: sil @$S9pgo_guard6guess11xs5Int32VAE_tF : $@convention(thin) (Int32) -> Int32 !function_entry_count(5002) {
// IR-LABEL: define swiftcc i32 @"$S9pgo_guard6guess11xs5Int32VAE_tF"
// IR-OPT-LABEL: define swiftcc i32 @"$S9pgo_guard6guess11xs5Int32VAE_tF"

public func guess1(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(5000) !false_count(2)
  // SIL-OPT: cond_br {{.*}} !true_count(5000) !false_count(2)

  // IR: br {{.*}}, !prof ![[BWMD:[0-9]+]]
  guard (x == 10) else {
    return 30
  }
  return 20
}


func main() {
  var guesses : Int32 = 0;

  guesses += guess1(x: 0)
  guesses += guess1(x: 42)

  for _ in 1...5000 {
    guesses += guess1(x: 10)
  }
}

main()

// IR: !{!"branch_weights", i32 5001, i32 3}
// IR-OPT: !{!"branch_weights", i32 5001, i32 3}
