// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache

// Run the scanner once, emitting the serialized scanner cache
// RUN: %target-swift-frontend -scan-dependencies -module-name FooReuse -module-load-mode prefer-interface -Rdependency-scan -validate-prior-dependency-scan-cache -serialize-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/clang-module-cache %s -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -o %t/deps_initial.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 -enable-cross-import-overlays &> %t/remarks_save.txt
// RUN: cat %t/remarks_save.txt | %FileCheck %s -check-prefix CHECK-REMARK-SAVE

// Run the scanner again, but now re-using previously-serialized cache
// RUN: %target-swift-frontend -scan-dependencies -module-name FooReuse -module-load-mode prefer-interface -Rdependency-scan -validate-prior-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/clang-module-cache %s -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 -enable-cross-import-overlays &> %t/remarks_load.txt
// RUN: cat %t/remarks_load.txt | %FileCheck %s -check-prefix CHECK-REMARK-LOAD

// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json &>/dev/null
// RUN: %FileCheck %s < %t/deps.json

// REQUIRES: executable_test
// REQUIRES: objc_interop

import C
import E
import G
import SubE

// CHECK-REMARK-SAVE: remark: Number of named Clang module queries: '6'
// CHECK-REMARK-SAVE: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '6'
// CHECK-REMARK-SAVE: remark: Number of recorded Swift module dependencies: '8'
// CHECK-REMARK-SAVE: remark: Number of recorded Clang module dependencies: '7'
// CHECK-REMARK-SAVE: remark: Incremental module scan: Serializing module scanning dependency cache to:

// CHECK-REMARK-LOAD: remark: Incremental module scan: Re-using serialized module scanning dependency cache from:
// CHECK-REMARK-LOAD: remark: Number of named Clang module queries: '0'
// CHECK-REMARK-LOAD: remark: Number of recorded Clang module dependencies queried by-name from a Swift client: '0'
// CHECK-REMARK-LOAD: remark: Number of recorded Swift module dependencies: '8'
// CHECK-REMARK-LOAD: remark: Number of recorded Clang module dependencies: '7'

// CHECK: "mainModuleName": "FooReuse"

/// --------Main module
// CHECK-LABEL: "modulePath": "FooReuse.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: module_deps_cache_reuse.swift
// CHECK-NEXT: ],
// CHECK-NEXT: "directDependencies": [
// CHECK-NEXT:   {
// CHECK-DAG:     "clang": "C"
// CHECK-DAG:     "swift": "E"
// CHECK-DAG:     "clang": "F"
// CHECK-DAG:     "swift": "G"
// CHECK-DAG:     "swift": "SubE"
// CHECK-DAG:     "swift": "_cross_import_E"
// CHECK: ],

// CHECK:      "contextHash":
// CHECK-NOT: "error: cannot open Swift placeholder dependency module map from"
// CHECK: "bridgingHeader":
// CHECK-NEXT: "path":
// CHECK-SAME: Bridging.h

// CHECK-NEXT: "sourceFiles":
// CHECK-NEXT: Bridging.h
// CHECK-NEXT: BridgingOther.h

// CHECK: "moduleDependencies": [
// CHECK-NEXT: "F"
// CHECK-NEXT: ]

// CHECK: "swiftOverlayDependencies": [
// CHECK-DAG: "swift": "F"
// CHECK-DAG: "swift": "A"

/// --------Clang module C
// CHECK-LABEL: "modulePath": "{{.*}}/C-{{.*}}.pcm",

// CHECK: "sourceFiles": [
// CHECK-DAG: module.modulemap
// CHECK-DAG: C.h

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK: "clang": "B"

// CHECK: "moduleMapPath"
// CHECK-SAME: module.modulemap

// CHECK: "contextHash"
// CHECK-SAME: "{{.*}}"

// CHECK: "commandLine": [
// CHECK-NEXT: "-frontend"
// CHECK-NOT: "BUILD_DIR/bin/clang"
// CHECK: "-emit-pcm",
// CHECK: "-module-name",
// CHECK-NEXT: "C"
// CHECK: "-direct-clang-cc1-module-build"

/// --------Clang module B
// CHECK-LABEL: "modulePath": "{{.*}}/B-{{.*}}.pcm",

// CHECK-NEXT: sourceFiles
// CHECK-DAG: module.modulemap
// CHECK-DAG: B.h

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK: "clang": "A"
// CHECK-NEXT: }

/// --------Swift module F
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}F-{{.*}}.swiftmodule",
// CHECK-NEXT: "sourceFiles": [
// CHECK-NEXT: ],
// CHECK-NEXT: "directDependencies": [
// CHECK-NEXT:   {
// CHECK:     "clang": "F"
// CHECK: ],

/// --------Swift module A
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}A-{{.*}}.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-DAG:   "clang": "A"

/// --------Swift module G
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}G-{{.*}}.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK:   "clang": "G"
// CHECK: ],
// CHECK-NEXT: "linkLibraries": [
// CHECK-NEXT: ],
// CHECK: "details": {

// CHECK: "commandLine": [
// CHECK: "-compile-module-from-interface"
// CHECK: "-target"
// CHECK: "-module-name"
// CHECK: "G"
// CHECK: "-swift-version"
// CHECK: "5"
// CHECK: ],
// CHECK: "contextHash": "{{.*}}",

/// --------Swift module E
// CHECK: "swift": "E"
// CHECK-LABEL: modulePath": "{{.*}}{{/|\\}}E-{{.*}}.swiftmodule"
// CHECK: "moduleInterfacePath"
// CHECK-SAME: E.swiftinterface
