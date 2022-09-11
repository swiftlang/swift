// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_lazy -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-codesign %t/main
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata

// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-sorted-sil -emit-sil -module-name pgo_lazy -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-sorted-sil -emit-sil -module-name pgo_lazy -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -emit-ir -module-name pgo_lazy -o - | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_lazy -o - | %FileCheck %s --check-prefix=IR

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

var cond = true

public struct S {
  // SIL-LABEL: sil [lazy_getter] [noinline] @$s8pgo_lazy1SV1xSivg : $@convention(method) (@inout S) -> Int !function_entry_count(35)
  // SIL: cond_br {{.*}} !true_count(5) !false_count(30)
  public lazy var x = cond ? 2 : 3
}

func triggerLazy() -> Int {
  var s = S()
  return s.x
}
public var total = 0
for _ in 0 ..< 5 {
  total += triggerLazy()
}
cond = false
for _ in 0 ..< 30 {
  total += triggerLazy()
}

// IR: !{!"branch_weights", i32 6, i32 31}
