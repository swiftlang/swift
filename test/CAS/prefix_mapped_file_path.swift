// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// The warnings are disabled by default, and only apply to files under a
/// prefix that the scanner substituted.
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %t/src/main.swift \
// RUN:   -cache-replay-prefix-map %t/src /original/src
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %t/src/main.swift \
// RUN:   -Wwarning PrefixMappedFilePath
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %t/src/main.swift \
// RUN:   -Wwarning PrefixMappedFilePath -cache-replay-prefix-map %t/sr /original/src
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 %t/src/main.swift \
// RUN:   -Wwarning PrefixMappedFilePath -cache-replay-prefix-map %t/src /original/src \
// RUN:   -verify-additional-prefix enabled- -verify-additional-prefix swift5-
// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 %t/src/main.swift \
// RUN:   -Wwarning PrefixMappedFilePath -cache-replay-prefix-map %t/src /original/src \
// RUN:   -verify-additional-prefix enabled-

/// With compilation caching, the warnings show the prefix-mapped path and are
/// replayed from the cache.
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/src/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -scanner-prefix-map-paths %t/src /^src
// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -emit-module -o %t/Test.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name Test -O -Wwarning PrefixMappedFilePath -Rcache-compile-job \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   /^src/main.swift @%t/MyApp.cmd -cache-replay-prefix-map /^src %t/src > %t/miss.txt 2>&1
// RUN: %FileCheck %s --input-file=%t/miss.txt
// RUN: %FileCheck %s --check-prefix=MISS --input-file=%t/miss.txt
// RUN: %target-swift-frontend-plain -emit-module -o %t/Test.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name Test -O -Wwarning PrefixMappedFilePath -Rcache-compile-job \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   /^src/main.swift @%t/MyApp.cmd -cache-replay-prefix-map /^src %t/src > %t/hit.txt 2>&1
// RUN: %FileCheck %s --input-file=%t/hit.txt
// RUN: %FileCheck %s --check-prefix=HIT --input-file=%t/hit.txt

// MISS: remark: cache miss for input
// HIT: remark: replay output file '<cached-diagnostics>'
// CHECK-DAG: main.swift:1:5: warning: '#filePath' evaluates to the prefix-mapped path '/^src/main.swift', not the original path of the source file
// CHECK-DAG: main.swift:2:5: warning: '#file' evaluates to the prefix-mapped path '/^src/main.swift', not the original path of the source file
// CHECK-DAG: main.swift:3:11: warning: '#filePath' evaluates to the prefix-mapped path '/^src/main.swift', not the original path of the source file
// CHECK-DAG: main.swift:4:13: warning: '#filePath' evaluates to the prefix-mapped path '/^src/main.swift', not the original path of the source file
// CHECK-DAG: main.swift:10:34: warning: '#filePath' evaluates to the prefix-mapped path '/^src/main.swift', not the original path of the source file

//--- src/main.swift
_ = #filePath // expected-enabled-warning {{'#filePath' evaluates to the prefix-mapped path}}
_ = #file // expected-swift5-warning {{'#file' evaluates to the prefix-mapped path}}
_ = "in \(#filePath)" // expected-enabled-warning {{'#filePath' evaluates to the prefix-mapped path}}
_ = { () in #filePath }() // expected-enabled-warning {{'#filePath' evaluates to the prefix-mapped path}}
_ = #fileID
_ = #function

// A default argument is diagnosed where it's written, not where the call
// site evaluates it.
func helper(file: StaticString = #filePath) {} // expected-enabled-warning {{'#filePath' evaluates to the prefix-mapped path}}
helper()

#sourceLocation(file: "/elsewhere/virtual.swift", line: 1)
_ = #filePath
#sourceLocation()
