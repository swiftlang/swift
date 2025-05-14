// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/cas

// RUN: not %target-swift-frontend -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o 2>&1 | %FileCheck %s --check-prefix=NO-CASFS
// NO-CASFS: caching is enabled without CAS file-system options

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd > %t/cache_key.json
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action validate-outputs %t/cache_key.json

/// make sure validate fails if the cas is cleared.
// RUN: rm -rf %t/cas
// RUN: not %cache-tool -cas-path %t/cas -cache-tool-action validate-outputs %t/cache_key.json 2>&1 | %FileCheck %s

// CHECK: failed to find output for cache key

func testFunc() {}
