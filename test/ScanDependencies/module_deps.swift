// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 -disable-implicit-swift-modules -Xcc -Xclang -Xcc -fno-implicit-modules

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// Check the make-style dependencies file
// RUN: %FileCheck %s -check-prefix CHECK-MAKE-DEPS < %t/deps.d

// Check that the JSON parses correctly into the canonical Swift data
// structures.

// RUN: mkdir -p %t/PrintGraph
// RUN: cp %S/Inputs/PrintGraph.swift %t/main.swift
// RUN: %target-build-swift %S/Inputs/ModuleDependencyGraph.swift %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/deps.json

// RUN: mkdir -p %t/BuildModules
// RUN: cp %S/Inputs/BuildModulesFromGraph.swift %t/BuildModules/main.swift
// RUN: %target-build-swift %S/Inputs/ModuleDependencyGraph.swift %t/BuildModules/main.swift -o %t/ModuleBuilder
// RUN: %target-codesign %t/ModuleBuilder

// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path SwiftShims.pcm -o %t/clang-module-cache/SwiftShims.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/SwiftShims.pcm
// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path A.pcm -o %t/clang-module-cache/A.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/A.pcm
// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path B.pcm -o %t/clang-module-cache/B.pcm -Xcc -Xclang -Xcc -fmodule-map-file=%S/Inputs/CHeaders/module.modulemap -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/A.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/B.pcm
// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path C.pcm -o %t/clang-module-cache/C.pcm -Xcc -Xclang -Xcc -fmodule-map-file=%S/Inputs/CHeaders/module.modulemap -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/B.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/C.pcm

// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path A.swiftmodule -o %t/clang-module-cache/A.swiftmodule -Xcc -Xclang -Xcc -fmodule-map-file=%S/Inputs/CHeaders/module.modulemap -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/A.pcm -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/SwiftShims.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/A.swiftmodule

// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path E.swiftmodule -o %t/clang-module-cache/E.swiftmodule -Xcc -Xclang -Xcc -fmodule-map-file=%S/Inputs/CHeaders/module.modulemap -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/SwiftShims.pcm | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/E.swiftmodule

// RUN: %target-run %t/ModuleBuilder %t/deps.json %swift-path SubE.swiftmodule -o %t/clang-module-cache/SubE.swiftmodule -Xcc -Xclang -Xcc -fmodule-map-file=%S/Inputs/CHeaders/module.modulemap -Xcc -Xclang -Xcc -fmodule-file=%t/clang-module-cache/SwiftShims.pcm -swift-module-file %t/clang-module-cache/E.swiftmodule | %S/Inputs/CommandRunner.py
// RUN: ls %t/clang-module-cache/SubE.swiftmodule

// REQUIRES: executable_test
// REQUIRES: objc_interop

import C
import E
import G
import SubE

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: module_deps.swift

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "C"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "E"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "G"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "SubE"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "SwiftOnoneSupport"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "F"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "A"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "_cross_import_E"
// CHECK-NEXT: }

// CHECK: "bridgingHeader":
// CHECK-NEXT: "path":
// CHECK-SAME: Bridging.h

// CHECK-NEXT: "sourceFiles":
// CHECK-NEXT: Bridging.h
// CHECK-NEXT: BridgingOther.h

// CHECK: "moduleDependencies": [
// CHECK-NEXT: "F"
// CHECK-NEXT: ]

/// --------Clang module C
// CHECK-LABEL: "modulePath": "C.pcm",

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

// CHECK: "commandLine": [
// CHECK-NEXT: "-frontend"
// CHECK-NEXT: "-only-use-extra-clang-opts"
// CHECK-NEXT: "-Xcc"
// CHECK-NEXT: "clang"
// CHECK: "-fno-implicit-modules"

/// --------Swift module E
// CHECK: "swift": "E"
// CHECK-LABEL: modulePath": "E.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"

// CHECK: "moduleInterfacePath"
// CHECK-SAME: E.swiftinterface

/// --------Swift module G
// CHECK-LABEL: "modulePath": "G.swiftmodule"
// CHECK: "directDependencies"
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "Swift"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "clang": "G"
// CHECK-NEXT: }
// CHECK-NEXT: ],
// CHECK-NEXT: "details": {

// CHECK: "contextHash": "{{.*}}",
// CHECK: "commandLine": [
// CHECK: "-compile-module-from-interface"
// CHECK: "-target"
// CHECK: "-sdk"
// CHECK: "-module-name"
// CHECK: "G"
// CHECK: "-swift-version"
// CHECK: "5"
// CHECK: ]

/// --------Swift module Swift
// CHECK-LABEL: "modulePath": "Swift.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "SwiftShims"

/// --------Swift module A
// CHECK-LABEL: "modulePath": "A.swiftmodule",

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "Swift"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "clang": "A"
// CHECK-NEXT: }

/// --------Clang module B
// CHECK-LABEL: "modulePath": "B.pcm"

// CHECK-NEXT: sourceFiles
// CHECK-DAG: module.modulemap
// CHECK-DAG: B.h

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "clang": "A"
// CHECK-NEXT: }

/// --------Clang module SwiftShims
// CHECK-LABEL: "modulePath": "SwiftShims.pcm",


// Check make-style dependencies
// CHECK-MAKE-DEPS: module_deps.swift
// CHECK-MAKE-DEPS-SAME: A.swiftinterface
// CHECK-MAKE-DEPS-SAME: G.swiftinterface
// CHECK-MAKE-DEPS-SAME: Swift.swiftmodule
// CHECK-MAKE-DEPS-SAME: B.h
// CHECK-MAKE-DEPS-SAME: F.h
// CHECK-MAKE-DEPS-SAME: Bridging.h
// CHECK-MAKE-DEPS-SAME: BridgingOther.h
// CHECK-MAKE-DEPS-SAME: module.modulemap
