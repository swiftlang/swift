// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t /^test

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/Test.cmd

// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job /^test/test.swift \
// RUN:   -emit-module-path %t/Test.swiftmodule -c -module-name Test -o %t/test.o -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -cache-replay-prefix-map /^test %t \
// RUN:   @%t/Test.cmd > %t/key.casid

/// Cache miss
// RUN: %target-swift-frontend-plain -cache-compile-job /^test/test.swift \
// RUN:   -emit-module-path %t/Test.swiftmodule -c -module-name Test -o %t/test.o -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -cache-replay-prefix-map /^test %t \
// RUN:   @%t/Test.cmd 2>&1 | %FileCheck %s

/// Cache hit
// RUN: %target-swift-frontend-plain -cache-compile-job /^test/test.swift \
// RUN:   -emit-module-path %t/Test.swiftmodule -c -module-name Test -o %t/test.o -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -cache-replay-prefix-map /^test %t \
// RUN:   @%t/Test.cmd 2>&1 | %FileCheck %s

/// Cache hit from libSwiftScan
// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -- \
// RUN:   %target-swift-frontend-plain -cache-compile-job /^test/test.swift \
// RUN:   -emit-module-path %t/Test.swiftmodule -c -module-name Test -o %t/test.o -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -cache-replay-prefix-map /^test %t \
// RUN:   @%t/Test.cmd 2>&1 | %FileCheck %s

// CHECK: TMP_DIR{{/|\\}}test.swift:1:10: warning: this is a warning

//--- test.swift
#warning("this is a warning")
