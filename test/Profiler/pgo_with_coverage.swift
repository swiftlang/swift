// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_with_coverage -o %t/main

// This unusual use of 'sh' allows the path of the profraw file to be
// substituted by %target-run.
// RUN: %target-codesign %t/main
// RUN: %target-run sh -c 'env LLVM_PROFILE_FILE=$1 $2' -- %t/default.profraw %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata

// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -profile-use=%t/default.profdata -emit-ir -module-name pgo_with_coverage | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_with_coverage | %FileCheck %s --check-prefix=IR

// REQUIRES: profile_runtime
// REQUIRES: executable_test
// REQUIRES: OS=macosx

// Make sure we don't try and profile implicit decls during PGO.
// IR-NOT: no profile data available

struct S {
  var x = .random() ? 2 : 3
}
print(S().x)
