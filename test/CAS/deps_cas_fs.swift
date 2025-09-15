// REQUIRES: objc_interop, OS=macosx

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/cas

// RUN: mkdir -p %t/resource/macosx
// RUN: cp %S/../IRGen/Inputs/legacy_type_info/a.yaml %t/resource/macosx/layouts-x86_64.yaml

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/../ScanDependencies/Inputs/CHeaders -I %S/../ScanDependencies/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/../ScanDependencies/Inputs/CHeaders/Bridging.h -swift-version 4 -cache-compile-job -cas-path %t/cas -module-name Test -resource-dir %t/resource -scanner-output-dir %t
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json &>/dev/null

/// check cas-fs content
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json E casFSRootID > %t/E_fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/E_fs.casid | %FileCheck %s -check-prefix FS_ROOT_E
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json F casFSRootID > %t/F_fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/F_fs.casid | %FileCheck %s -check-prefix FS_ROOT_F
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test casFSRootID > %t/Test_fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/Test_fs.casid | %FileCheck %s -check-prefix FS_ROOT_TEST

// FS_ROOT_E-DAG: layouts-x86_64.yaml
// FS_ROOT_E-DAG: E.swiftinterface

// FS_ROOT_F-DAG: layouts-x86_64.yaml
// FS_ROOT_F-DAG: F.swiftinterface

// FS_ROOT_TEST-DAG: layouts-x86_64.yaml
// FS_ROOT_TEST-DAG: deps_cas_fs.swift

import E
import SubE
