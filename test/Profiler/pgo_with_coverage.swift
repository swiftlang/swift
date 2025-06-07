// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -Xfrontend -disable-incremental-llvm-codegen -module-name pgo_with_coverage -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata

// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -profile-use=%t/default.profdata -emit-ir -module-name pgo_with_coverage | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend %s -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -profile-use=%t/default.profdata -O -emit-ir -module-name pgo_with_coverage | %FileCheck %s --check-prefix=IR

// REQUIRES: profile_runtime
// REQUIRES: executable_test

// Make sure we don't try and profile implicit decls during PGO.
// IR-NOT: no profile data available

struct S {
  var x = .random() ? 2 : 3
}
print(S().x)
