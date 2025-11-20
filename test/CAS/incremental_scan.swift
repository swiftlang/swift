// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/Swift -cache-compile-job -cas-path %t/cas -no-clang-include-tree -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE --check-prefix=FAILED-LOAD

/// Test reuse
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/Swift -cache-compile-job -cas-path %t/cas -no-clang-include-tree -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE

/// Test invalidation after removing CAS.
// RUN: rm -rf %t/cas
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/Swift -cache-compile-job -cas-path %t/cas -no-clang-include-tree -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE --check-prefix=INVALIDATE

// REUSE: Incremental module scan: Re-using serialized module scanning dependency cache from:
// FAILED-LOD: Incremental module scan: Failed to load module scanning dependency cache from
// INVALIDATE: Incremental module scan: Dependency info for module '{{.*}}' invalidated due to missing CAS input
// INVALIDATE: Incremental module scan: Dependency info for module '{{.*}}' invalidated due to an out-of-date dependency.
// REUSE: Incremental module scan: Serializing module scanning dependency cache to
import E
