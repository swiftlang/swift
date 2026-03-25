// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Scan with minimized source enabled
// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/Test.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -language-mode 5 -enable-library-evolution -scan-dependencies-minimized-source

// Check that the source module's command line includes -parse-as-interface
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test commandLine > %t/cmd.json
// RUN: %FileCheck %s --input-file=%t/cmd.json

// CHECK: "-parse-as-interface"

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -cache-compile-job %t/Test.swift \
// RUN:   -emit-module -emit-module-path %t/Test.swiftmodule \
// RUN:   -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -module-name Test -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   @%t/MyApp.cmd -swift-version 5 -language-mode 5 -enable-library-evolution -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=MISS

/// Append private function and expect cache hit.
// RUN: echo "internal func internalFunc() {}" >> %t/Test.swift

// RUN: %target-swift-frontend-plain -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/Test.swift -o %t/deps2.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -language-mode 5 -enable-library-evolution -scan-dependencies-minimized-source
// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps2.json -o %t/MyApp2.cmd

// RUN: %target-swift-frontend-plain -cache-compile-job %t/Test.swift \
// RUN:   -emit-module -emit-module-path %t/Test.swiftmodule \
// RUN:   -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -module-name Test -O -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   @%t/MyApp2.cmd -swift-version 5 -language-mode 5 -enable-library-evolution -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=HIT

// MISS: remark: cache miss for input file
// HIT: remark: replay output file

//--- Test.swift
public func publicFunc() -> Int { return 42 }

private func privateHelper() -> Int { return 0 }
