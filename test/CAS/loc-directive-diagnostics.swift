// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -emit-module -emit-module-path %t/test.module \
// RUN:   -cache-compile-job -cas-path %t/cas @%t/MyApp.cmd %s 2>&1 | %FileCheck %s
// RUN: %cache-tool print-output-keys -cas-path %t/cas -- \
// RUN:   %target-swift-frontend-plain -emit-module -emit-module-path %t/test.module -cache-compile-job -cas-path %t/cas \
// RUN:   @%t/MyApp.cmd %s > %t/cache_key.json
// RUN: %cache-tool render-diags -cas-path %t/cas %t/cache_key.json --  \
// RUN:   %target-swift-frontend-plain -emit-module -emit-module-path %t/test.module -cache-compile-job -cas-path %t/cas \
// RUN:   @%t/MyApp.cmd %s 2>&1 | %FileCheck %s

#sourceLocation(file: "anything.swift", line: 1)
#warning("this is a warning")
#sourceLocation()

// CHECK: anything.swift:1:10: warning: this is a warning
