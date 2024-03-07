// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -g \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: echo %t/main.swift > %t/inputs.FileList
// RUN: %target-swift-frontend -emit-ir -o %t/main.ll -g -O \
// RUN:   -cache-compile-job -cas-path %t/cas -swift-version 5 \
// RUN:   -disable-implicit-swift-modules -swift-version 5 -enable-cross-import-overlays \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -filelist %t/inputs.FileList @%t/MyApp.cmd -debug-info-store-invocation -Rcache-compile-job

// RUN: %FileCheck %s --input-file=%t/main.ll

// CHECK: distinct !DICompileUnit(language: DW_LANG_Swift,
// CHECK-SAME: flags:
// CHECK-SAME: -filelist

/// option that should not be encoded
// CHECK-NOT: -Rcache-compile-job
// CHECK-NOT: inputs.FileList

//--- main.swift
public func test() {}
