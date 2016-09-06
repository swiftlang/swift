// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_if -o %t/main
// RUN: env LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_if -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_if -o - | %FileCheck %s --check-prefix=IR

// REQUIRES: profile_runtime
// REQUIRES: OS=macosx

// SIL-LABEL: // pgo_if.guess1
// IR-LABEL: define i32 @_TF6pgo_if6guess1
public func guess1(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(5000) !false_count(1)

  // IR: %1 = icmp eq i32 %0, 1
  // IR-NEXT : br {{.*}}, !prof ![[BWMD:[0-9]+]]
  if (x == 1) {
    return 0
  } else {
    return 1
  }
}

// SIL-LABEL: // pgo_if.guess2
// IR-LABEL: define i32 @_TF6pgo_if6guess2
public func guess2(x: Int32) -> Int32 {
  // SIL: cond_br {{.*}} !true_count(5000) !false_count(1)

  // IR: %1 = icmp eq i32 %0, 1
  // IR-NEXT : br {{.*}}, !prof ![[BWMD]]
  return (x == 1) ? 0 : 1
}

func main() {
  var guesses : Int32 = 0;

  guesses += guess1(x: 0) + guess2(x: 0)

  for _ in 1...5000 {
    guesses += guess1(x: 1) + guess2(x: 1)
  }
}

main()

// IR: !{!"branch_weights", i32 5001, i32 2}
