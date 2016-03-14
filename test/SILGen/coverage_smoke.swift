// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -o %t/main
// RUN: env LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main
// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-profdata show %t/default.profdata -function=main | FileCheck %s --check-prefix=CHECK-PROF
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | FileCheck %s --check-prefix=CHECK-COV
// RUN: rm -rf %t
// REQUIRES: profile_runtime
// REQUIRES: OS=macosx

func main() {
// CHECK-PROF: Counters: 2
// CHECK-PROF: Function count: 1
// CHECK-COV: 1|{{.*}}[[@LINE+1]]|{{.*}}if (true)
  if (true) {}
}

main()
