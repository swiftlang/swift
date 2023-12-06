// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Test compile multiple inputs with batch mode.
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift %t/foo.swift -emit-module -o %t/Test.swiftmodule \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job -primary-file %t/test.swift %t/foo.swift -c -o %t/test.o  \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -primary-file %t/foo.swift -c -o %t/foo.o  \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

/// Expect cache hit second time
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift %t/foo.swift -emit-module -o %t/Test.swiftmodule \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job -primary-file %t/test.swift %t/foo.swift -c -o %t/test.o  \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s
// RUN: %target-swift-frontend -cache-compile-job -Rcache-compile-job %t/test.swift -primary-file %t/foo.swift -c -o %t/foo.o  \
// RUN:  -module-name Test -cas-path %t/cas -allow-unstable-cache-key-for-testing 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s

//--- test.swift
func testFunc() {}

//--- foo.swift
func foo() {}

// CACHE-MISS: remark: cache miss for input
// CACHE-MISS-NOT: remark: replay output file
// CACHE-HIT: remark: replay output file
// CACHE-HIT-NOT: remark: cache miss for input
