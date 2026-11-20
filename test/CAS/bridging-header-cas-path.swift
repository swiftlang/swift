// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// The CAS path should not affect the cache key of the bridging header PCH job.

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps-1.json -cache-compile-job -cas-path %t/cas -import-objc-header %t/Bridging.h

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-1.json bridgingHeader > %t/header-1.cmd
// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/header-1.cmd

// CMD-NOT: "-fcas-path"

// RUN: %target-swift-frontend-plain @%t/header-1.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/bridging.pch \
// RUN:   -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS
// RUN: %cache-tool print-output-keys -cas-path %t/cas -- \
// RUN:   %target-swift-frontend-plain @%t/header-1.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/bridging.pch > %t/keys-1.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys-1.json > %t/key-1

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file
// CACHE-HIT-NOT: remark: cache miss for input

/// Move the CAS to a different path. The PCH job should get the same key and hit the cache.
// RUN: mv %t/cas %t/cas-2

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps-2.json -cache-compile-job -cas-path %t/cas-2 -import-objc-header %t/Bridging.h

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-2.json bridgingHeader > %t/header-2.cmd
// RUN: %target-swift-frontend-plain @%t/header-2.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/bridging.pch \
// RUN:   -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT
// RUN: %cache-tool print-output-keys -cas-path %t/cas-2 -- \
// RUN:   %target-swift-frontend-plain @%t/header-2.cmd %t/Bridging.h -disable-implicit-swift-modules -o %t/bridging.pch > %t/keys-2.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys-2.json > %t/key-2
// RUN: diff %t/key-1 %t/key-2

//--- test.swift
public func test() {}

//--- Bridging.h
static inline int bridged(void) { return 42; }
