// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -scanner-prefix-map-paths %t /^test \
// RUN:   -locale en
// RUN: %FileCheck %s --check-prefix=DEPS --input-file=%t/deps.json

// DEPS: "-localization-path",
// DEPS-NEXT: "{{.*}}share{{.*}}swift{{.*}}diagnostics"

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/test.cmd
// RUN: not %target-swift-frontend-plain -cache-compile-job -module-name Test -cas-path %t/cas @%t/test.cmd \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -disable-implicit-swift-modules -parse-stdlib -emit-module -o %t/test.swiftmodule /^test/test.swift \
// RUN:   -locale en 2>&1 | %FileCheck %s

// CHECK-NOT: specified localization directory
// CHECK-NOT: cannot find translations
// CHECK: error: unterminated string literal

//--- test.swift
_ = "HI!
