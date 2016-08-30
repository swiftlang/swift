// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_if -o %t/main
// RUN: env LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_if -o - | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: OS=macosx

// CHECK-LABEL: // pgo_if.guess1
public func guess1(x: Int32) -> Int32 {
  // CHECK: cond_br {{.*}} !true_count(100) !false_count(0)
  if (x == 1) {
    return 0
  } else {
    return 1
  }
}

// CHECK-LABEL: // pgo_if.guess2
public func guess2(x: Int32) -> Int32 {
  // CHECK: cond_br {{.*}} !true_count(100) !false_count(0)
  return (x == 1) ? 0 : 1
}

func main() {
  var guesses : Int32 = 0;
  for _ in 1...100 {
    guesses += guess1(x: 1)
    guesses += guess2(x: 1)
  }
}

main()
