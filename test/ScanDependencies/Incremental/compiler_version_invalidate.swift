// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// Initial scan with a stable, simulated compiler version.
// RUN: env SWIFT_DEBUG_FORCE_DEPENDENCY_SCAN_VERSION=v1 %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../Inputs/CHeaders -module-name Test &> %t/v1_initial.txt
// RUN: cat %t/v1_initial.txt | %FileCheck %s -check-prefix=V1-INITIAL

// Re-scan with the same compiler version reuses the cache.
// RUN: env SWIFT_DEBUG_FORCE_DEPENDENCY_SCAN_VERSION=v1 %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../Inputs/CHeaders -module-name Test &> %t/v1_rescan.txt
// RUN: cat %t/v1_rescan.txt | %FileCheck %s -check-prefix=V1-RESCAN

// Re-scan with a different compiler version rejects the cache.
// RUN: env SWIFT_DEBUG_FORCE_DEPENDENCY_SCAN_VERSION=v2 %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../Inputs/CHeaders -module-name Test &> %t/v2_after_v1.txt
// RUN: cat %t/v2_after_v1.txt | %FileCheck %s -check-prefix=V2-AFTER-V1

// Re-scan again at v2 reuses the freshly-written cache.
// RUN: env SWIFT_DEBUG_FORCE_DEPENDENCY_SCAN_VERSION=v2 %target-swift-frontend -scan-dependencies -scanner-module-validation -module-load-mode prefer-interface -Rdependency-scan-cache -load-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/../Inputs/CHeaders -module-name Test &> %t/v2_rescan.txt
// RUN: cat %t/v2_rescan.txt | %FileCheck %s -check-prefix=V2-RESCAN

// V1-INITIAL: remark: Incremental module scan: Failed to load module scanning dependency cache from: {{.*}}cache.moddepcache
// V1-INITIAL: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

// V1-RESCAN: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// V1-RESCAN-NOT: remark: Incremental module scan: Failed to load module scanning dependency cache from:

// V2-AFTER-V1: remark: Incremental module scan: Failed to load module scanning dependency cache from: {{.*}}cache.moddepcache
// V2-AFTER-V1: remark: Incremental module scan: Serializing module scanning dependency cache to: {{.*}}cache.moddepcache

// V2-RESCAN: remark: Incremental module scan: Re-using serialized module scanning dependency cache from: {{.*}}cache.moddepcache
// V2-RESCAN-NOT: remark: Incremental module scan: Failed to load module scanning dependency cache from:
