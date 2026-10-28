/// Check the reproducer for a compilation whose input is provided by
/// `-input-file-key`: the input is captured on disk instead, so the reproducer
/// runs without the original file and without the cache key.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/cas
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %t/Test.swift -o %t/deps.json -module-name Test \
// RUN:   -swift-version 5 -cache-compile-job -cas-path %t/cas -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

/// Emit the module interface into the CAS and extract the cache key of that output.
// RUN: %target-swift-frontend-plain -emit-module -emit-module-path %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -disable-implicit-swift-modules -module-cache-path %t/module-cache -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/Test.swift -cache-compile-job -cas-path %t/cas -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import @%t/MyApp.cmd
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend-plain -emit-module -emit-module-path %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -disable-implicit-swift-modules -module-cache-path %t/module-cache -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/Test.swift -cache-compile-job -cas-path %t/cas -swift-version 5 -enable-library-evolution -parse-stdlib \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import @%t/MyApp.cmd > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json %t/Test.swift > %t/key

// RUN: %target-swift-frontend-plain -typecheck-module-from-interface %t/Test.swiftinterface -disable-implicit-swift-modules \
// RUN:   -module-cache-path %t/module-cache -explicit-swift-module-map-file @%t/map.casid -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -enable-library-evolution -parse-stdlib -explicit-interface-module-build \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   @%t/MyApp.cmd -input-file-key @%t/key -gen-reproducer -gen-reproducer-dir %t/crash

/// The interface is captured on disk and the cache key is no longer needed.
// RUN: %FileCheck %s --input-file=%t/crash/reproduce.sh
// CHECK-NOT: -input-file-key
// CHECK: "inputs{{.*}}Test.swiftinterface"
// CHECK: "-cas-fs-input-overlay"

// RUN: rm %t/Test.swiftinterface
// RUN: cd %t/crash && %swift_frontend_plain @reproduce.sh

/// The captured interface takes precedence over what the CAS provides. Note
/// `%:t` is the layout the reproducer mirrors: the path of `%t` with the root
/// turned into a regular directory, e.g. `C:/dir` becomes `C/dir`.
// RUN: echo "public func added() -> NotAType {}" >> %t/crash/inputs/%:t/Test.swiftinterface
// RUN: cd %t/crash && not %swift_frontend_plain @reproduce.sh 2>&1 | %FileCheck %s --check-prefix=EDITED
// EDITED: error: cannot find type 'NotAType' in scope

//--- Test.swift
public func test() {}
