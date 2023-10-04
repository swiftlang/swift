// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Run the command first time, expect cache miss.
/// FIXME: This command doesn't use `-cas-fs` so it is not a good cache entry. It is currently allowed so it is easier to write tests.
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -emit-module -emit-module-path %t/Test.swiftmodule -c -emit-dependencies \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

/// Expect cache hit for second time.
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -emit-module -emit-module-path %t/Test.swiftmodule -c -emit-dependencies \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s

/// Expect cache miss a subset of outputs.
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

/// Cache hit for retry.
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s

/// Skip cache
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job -cache-disable-replay %t/test.swift -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --allow-empty --check-prefix=SKIP-CACHE %s

/// Check clang module
// RUN: %target-swift-frontend -emit-pcm -module-name Dummy %t/module.modulemap -cache-compile-job -Rcache-compile-job -cas-path %t/cas \
// RUN:   -allow-unstable-cache-key-for-testing -o %t/dummy.pcm 2>&1 | %FileCheck --allow-empty --check-prefix=CACHE-MISS %s
// RUN: %target-swift-frontend -emit-pcm -module-name Dummy %t/module.modulemap -cache-compile-job -Rcache-compile-job -cas-path %t/cas \
// RUN:   -allow-unstable-cache-key-for-testing -o %t/dummy.pcm 2>&1 | %FileCheck --allow-empty --check-prefix=CACHE-HIT-CLANG %s

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file '<cached-diagnostics>': key 'llvmcas://{{.*}}'
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}test.o': key 'llvmcas://{{.*}}'
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}Test.swiftmodule': key 'llvmcas://{{.*}}'
// CACHE-HIT-CLANG: remark: replay output file '<cached-diagnostics>': key 'llvmcas://{{.*}}'
// CACHE-HIT-CLANG: remark: replay output file '{{.*}}{{/|\\}}dummy.pcm': key 'llvmcas://{{.*}}'
// SKIP-CACHE-NOT: remark:

//--- test.swift
func testFunc() {}

//--- module.modulemap
module Dummy {
 umbrella header "Dummy.h"
}

//--- Dummy.h
void dummy(void);
