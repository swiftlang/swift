// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name A -o %t/A.swiftmodule -swift-version 5 \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/A.swift

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas  \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE --check-prefix=FAILED-LOAD

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas  \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE

/// Test invalidation after removing CAS.
// RUN: rm -rf %t/cas
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas  \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE --check-prefix=INVALIDATE

/// Test a swiftmodule in CAS but not in action cache.
// RUN: rm -rf %t/cas
// RUN: llvm-cas -cas %t/cas -make-blob -data %t/A.swiftmodule
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/Test.swift -o %t/deps.json -I %t -cache-compile-job -cas-path %t/cas  \
// RUN:   -Rdependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache \
// RUN:   -serialize-dependency-scan-cache -validate-prior-dependency-scan-cache 2>&1 | %FileCheck %s --check-prefix=REUSE --check-prefix=INVALIDATE

// REUSE: Incremental module scan: Re-using serialized module scanning dependency cache from:
// FAILED-LOD: Incremental module scan: Failed to load module scanning dependency cache from
// INVALIDATE: Incremental module scan: Dependency info for module 'A' invalidated due to missing CAS input
// INVALIDATE: Incremental module scan: Dependency info for module 'deps' invalidated due to an out-of-date dependency.
// REUSE: Incremental module scan: Serializing module scanning dependency cache to

//--- A.swift
public func a() {}

//--- Test.swift
import A
