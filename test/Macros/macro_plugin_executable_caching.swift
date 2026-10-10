// REQUIRES: swift_swift_parser

/// Test that an executable plugin loaded through `-load-plugin-executable` is
/// captured in the CAS file system and verified like a library plugin.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
// RUN: split-file %s %t

// RUN: %swift-build-c-plugin -o %t/plugins/mock-plugin %t/plugin.c

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas \
// RUN:   -load-plugin-executable %t/plugins/mock-plugin#TestPlugin \
// RUN:   -scanner-prefix-map-paths %t /^test -resolved-plugin-verification

/// The executable is in the CAS file system.
// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json MyApp casFSRootID > %t/fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/fs.casid | %FileCheck %s --check-prefix=FS
// FS: /^test{{/|\\}}plugins{{/|\\}}mock-plugin

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd
// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/MyApp.cmd

// CMD: -resolved-plugin-verification
// CMD-NEXT: -load-resolved-plugin
// CMD-NEXT: #/^test{{/|\\}}plugins{{/|\\}}mock-plugin#TestPlugin

// RUN: %target-swift-frontend-plain \
// RUN:   -typecheck -verify -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   /^test/macro.swift @%t/MyApp.cmd -cache-replay-prefix-map /^test %t

/// Changing the executable fails the verification.
// RUN: echo " " >> %t/plugins/mock-plugin
// RUN: not %target-swift-frontend-plain \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -module-name MyApp -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   /^test/macro.swift @%t/MyApp.cmd -cache-replay-prefix-map /^test %t 2>&1 | %FileCheck %s --check-prefix=FAILED
// FAILED: resolved macro plugin '{{.*}}mock-plugin' failed verification: plugin has changed since dependency scanning

//--- macro.swift
@freestanding(expression) macro testString(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")

func test() {
  let _: String = #testString(123)
}

//--- plugin.c
#include "swift-c/MockPlugin/MockPlugin.h"

MOCK_PLUGIN([
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 1}}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestStringMacro"},
                "syntax": {"kind": "expression", "source": "#testString(123)"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "\"123\"", "diagnostics": []}}
  }
])
