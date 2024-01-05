// RUN: %empty-directory(%t)

// RUN: %swift-scan-test -action compute_cache_key -cas-path %t/cas -input %s -- %target-swift-frontend -cache-compile-job -Rcache-compile-job %s \
// RUN:   -emit-module -emit-module-path %t/Test.swiftmodule -c -emit-dependencies -module-name Test -o %t/test.o -cas-path %t/cas \
// RUN:   -allow-unstable-cache-key-for-testing -Xllvm -aarch64-use-tbi > %t/key.casid

// RUN: not %swift-scan-test -action cache_query -id @%t/key.casid -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=CHECK-QUERY-NOT-FOUND

// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %s -emit-module -emit-module-path %t/Test.swiftmodule -c -emit-dependencies \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing -Xllvm -aarch64-use-tbi

// RUN: %swift-scan-test -action cache_query -id @%t/key.casid -cas-path %t/cas | %FileCheck %s --check-prefix=CHECK-QUERY

// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -threads 10 -- %target-swift-frontend -cache-compile-job -Rcache-compile-job %s \
// RUN:   -emit-module -emit-module-path %t/Test2.swiftmodule -c -emit-dependencies -module-name Test -o %t/test2.o -cas-path %t/cas \
// RUN:   -allow-unstable-cache-key-for-testing -Xllvm -aarch64-use-tbi

// CHECK-QUERY-NOT-FOUND: cached output not found
// CHECK-QUERY: Cached Compilation for key "llvmcas://{{.*}}" has 4 outputs:
// CHECK-QUERY-NEXT: object: llvmcas://
// CHECK-QUERY-NEXT: dependencies: llvmcas://
// CHECK-QUERY-NEXT: swiftmodule: llvmcas://
// CHECK-QUERY-NEXT: cached-diagnostics: llvmcas://

func testFunc() {}
