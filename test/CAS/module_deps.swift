// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/cas

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/CHeaders -I %S/../ScanDependencies/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/../ScanDependencies/Inputs/CHeaders/Bridging.h -swift-version 4 -enable-cross-import-overlays -cache-compile-job -cas-path %t/cas -no-clang-include-tree
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json &>/dev/null
// RUN: %FileCheck -check-prefix CHECK -check-prefix CHECK_NO_CLANG_TARGET %s < %t/deps.json

// Check the contents of the JSON output
// RUN: %FileCheck %s -check-prefix CHECK -check-prefix CHECK-NO-SEARCH-PATHS < %t/deps.json

// Check the make-style dependencies file
// RUN: %FileCheck %s -check-prefix CHECK-MAKE-DEPS < %t/deps.d

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -test-dependency-scan-cache-serialization -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/CHeaders -I %S/../ScanDependencies/Inputs/Swift -import-objc-header %S/../ScanDependencies/Inputs/CHeaders/Bridging.h -swift-version 4 -enable-cross-import-overlays -cache-compile-job -cas-path %t/cas -no-clang-include-tree
// RUN: %validate-json %t/deps.json &>/dev/null
// RUN: %FileCheck -check-prefix CHECK -check-prefix CHECK_NO_CLANG_TARGET %s < %t/deps.json

// Ensure that scanning with `-clang-target` makes sure that Swift modules' respective PCM-dependency-build-argument sets do not contain target triples.
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -module-cache-path %t/clang-module-cache %s -o %t/deps_clang_target.json -I %S/../ScanDependencies/Inputs/CHeaders -I %S/../ScanDependencies/Inputs/Swift -import-objc-header %S/../ScanDependencies/Inputs/CHeaders/Bridging.h -swift-version 4 -enable-cross-import-overlays -clang-target %target-cpu-apple-macosx10.14 -cache-compile-job -cas-path %t/cas -no-clang-include-tree
// Check the contents of the JSON output
// RUN: %validate-json %t/deps_clang_target.json &>/dev/null
// RUN: %FileCheck -check-prefix CHECK_CLANG_TARGET %s < %t/deps_clang_target.json

/// check cas-fs content
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json E casFSRootID > %t/E_fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/E_fs.casid | %FileCheck %s -check-prefix FS_ROOT_E
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:F casFSRootID > %t/F_fs.casid
// RUN: llvm-cas --cas %t/cas --ls-tree-recursive @%t/F_fs.casid | %FileCheck %s -check-prefix FS_ROOT_F

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json deps commandLine > %t/deps.cmd
// RUN: %FileCheck %s -check-prefix MAIN_CMD -input-file=%t/deps.cmd

// FS_ROOT_E-DAG: E.swiftinterface
// FS_ROOT_E-DAG: SDKSettings.json

// FS_ROOT_F: CHeaders/A.h
// FS_ROOT_F: CHeaders/B.h
// FS_ROOT_F: CHeaders/C.h
// FS_ROOT_F: CHeaders/D.h
// FS_ROOT_F: CHeaders/F.h
// FS_ROOT_F: CHeaders/G.h
// FS_ROOT_F: CHeaders/H.h
// FS_ROOT_F: CHeaders/I.h
// FS_ROOT_F: CHeaders/X.h
// FS_ROOT_F: CHeaders/module.modulemap

// MAIN_CMD: -direct-clang-cc1-module-build
// MAIN_CMD: -cas-fs
// MAIN_CMD-NOT: -clang-include-tree-root

import C
import E
import G
import SubE

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: module_deps.swift
// CHECK-NEXT: ],
// CHECK-NEXT: "directDependencies": [
// CHECK-DAG:     "clang": "C"
// CHECK-DAG:     "swift": "E"
// CHECK-DAG:     "swift": "G"
// CHECK-DAG:     "swift": "SubE"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "swift": "SwiftOnoneSupport"
// CHECK-DAG:     "swift": "_Concurrency"
// CHECK-DAG:     "swift": "_cross_import_E"
// CHECK: ],
// CHECK:      "commandLine":
// CHECK:      "casFSRootID":
// CHECK:      "extraPcmArgs": [
// CHECK-NEXT:    "-Xcc",
// CHECK-NEXT:    "-target",
// CHECK-NEXT:    "-Xcc",
// CHECK:         "-fapinotes-swift-version=4"
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
// CHECK-DAG:     "swift": "A"
// CHECK-DAG:     "swift": "F"

/// --------Clang module C
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}C-{{.*}}.pcm",

// CHECK: "sourceFiles": [
// CHECK-DAG: module.modulemap
// CHECK-DAG: C.h

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "B"

// CHECK: "moduleMapPath"
// CHECK-SAME: module.modulemap

// CHECK: "contextHash"
// CHECK-SAME: "{{.*}}"

/// --------Clang module B
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}B-{{.*}}.pcm",
// CHECK: "contextHash": "[[B_CONTEXT:.*]]",
// CHECK: "-o"
// CHECK-NEXT: B-{{.*}}[[B_CONTEXT]].pcm

// Check make-style dependencies
// CHECK-MAKE-DEPS: module_deps.swift
// CHECK-MAKE-DEPS-SAME: A.swiftinterface
// CHECK-MAKE-DEPS-SAME: G.swiftinterface
// CHECK-MAKE-DEPS-SAME: B.h
// CHECK-MAKE-DEPS-SAME: F.h
// CHECK-MAKE-DEPS-SAME: Bridging.h
// CHECK-MAKE-DEPS-SAME: BridgingOther.h
// CHECK-MAKE-DEPS-SAME: module.modulemap

/// --------Swift module F
// CHECK:      "modulePath": "{{.*}}{{/|\\}}F-{{.*}}.swiftmodule",
// CHECK-NEXT: "sourceFiles": [
// CHECK-NEXT: ],
// CHECK-NEXT: "directDependencies": [
// CHECK-NEXT:   {
// CHECK-DAG:     "clang": "F"
// CHECK-DAG:     "swift": "Swift"
// CHECK-DAG:     "swift": "SwiftOnoneSupport"
// CHECK-NEXT:   }
// CHECK-NEXT: ],
// CHECK: "details":
// CHECK: "moduleCacheKey":

/// --------Swift module A
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}A-{{.*}}.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-DAG:   "clang": "A"
// CHECK-DAG:   "swift": "Swift"
// CHECK-DAG:   "swift": "SwiftOnoneSupport"
// CHECK-NEXT: }
// CHECK: "details":
// CHECK: "moduleCacheKey":

/// --------Swift module G
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}G-{{.*}}.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK-DAG:   "clang": "G"
// CHECK-DAG:   "swift": "Swift"
// CHECK-DAG:   "swift": "SwiftOnoneSupport"
// CHECK: ],
// CHECK-NEXT: "linkLibraries": [
// CHECK: "details": {

// CHECK: "commandLine": [
// CHECK: "-compile-module-from-interface"
// CHECK: "-target"
// CHECK: "-cache-compile-job"
// CHECK: "-cas-path"
// CHECK: "-module-name"
// CHECK: "G"
// CHECK: "-swift-version"
// CHECK: "5"
// CHECK: ],
// CHECK: "contextHash": "{{.*}}",
// CHECK_NO_CLANG_TARGET: "extraPcmArgs": [
// CHECK_NO_CLANG_TARGET-NEXT:   "-Xcc",
// CHECK_NO_CLANG_TARGET-NEXT:   "-target",
// CHECK_CLANG_TARGET: "extraPcmArgs": [
// CHECK_CLANG_TARGET-NEXT:   "-Xcc",
// CHECK_CLANG_TARGET-NEXT:   "-fapinotes-swift-version={{.*}}"
// CHECK_CLANG_TARGET-NEXT:   ]

/// --------Swift module E
// CHECK: "swift": "E"
// CHECK-LABEL: modulePath": "{{.*}}{{/|\\}}E-{{.*}}.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"

// CHECK: "moduleInterfacePath"
// CHECK-SAME: E.swiftinterface

/// --------Swift module Swift
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}Swift-{{.*}}.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "SwiftShims"

/// --------Clang module SwiftShims
// CHECK-LABEL: "modulePath": "{{.*}}{{/|\\}}SwiftShims-{{.*}}.pcm",
// CHECK: "contextHash": "[[SHIMS_CONTEXT:.*]]",
// CHECK: "-o"
// CHECK-NEXT: SwiftShims-{{.*}}[[SHIMS_CONTEXT]].pcm
// CHECK-NO-SEARCH-PATHS-NOT: "-prebuilt-module-cache-path"
