// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/cas

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json \
// RUN:  -I %S/../ScanDependencies/Inputs/CHeaders -I %S/../ScanDependencies/Inputs/Swift -emit-dependencies \
// RUN:  -import-objc-header %S/../ScanDependencies/Inputs/CHeaders/Bridging.h -swift-version 4 -cache-compile-job \
// RUN:  -cas-path %t/cas -scanner-prefix-map-paths %swift_src_root /^src -scanner-prefix-map-paths %t /^tmp -scanner-output-dir %t -auto-bridging-header-chaining

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json deps casFSRootID > %t/deps.fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/deps.fs.casid | %FileCheck %s -check-prefix DEPS-FS
// DEPS-FS: /^src/test/CAS/module_path_remap.swift

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json deps bridgingHeader | %FileCheck %s -check-prefix DEPS-BRIDGING
// DEPS-BRIDGING: -fmodule-file=F=/^tmp/clang-module-cache/F-{{.*}}.pcm

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json F casFSRootID > %t/F.fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/F.fs.casid | %FileCheck %s -check-prefix F-FS
// F-FS: /^src/test/CAS/../ScanDependencies/Inputs/Swift/F.swiftinterface

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json F commandLine | %FileCheck %s -check-prefix F-CMD
// F-CMD: /^src/test/CAS/../ScanDependencies/Inputs/Swift/F.swiftinterface
// F-CMD: -fmodule-file=SwiftShims=/^tmp/clang-module-cache/SwiftShims-{{.*}}.pcm

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:F clangIncludeTree > %t/tree.casid
// RUN: clang-cas-test --cas %t/cas --print-include-tree @%t/tree.casid | %FileCheck %s -check-prefix TREE
// TREE: /^src/test/CAS/../ScanDependencies/Inputs/CHeaders/F.h

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:F commandLine | %FileCheck %s -check-prefix CLANG-CMD
// CLANG-CMD: /^src/test/ScanDependencies/Inputs/CHeaders/module.modulemap


