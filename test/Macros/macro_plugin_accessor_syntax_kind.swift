// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-build-c-plugin -o %t/mock-plugin-v7 %t/plugin-v7.c
// RUN: %swift-build-c-plugin -o %t/mock-plugin-v8 %t/plugin-v8.c

// With protocol version 7 (< 8), AccessorDeclSyntax in the lexical context must
// be sent as "kind":"declaration" (the "accessor" kind is unknown to the plugin).
// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin-executable %t/mock-plugin-v7#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/output-v7.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-V7 %s < %t/output-v7.txt

// With protocol version 8, AccessorDeclSyntax in the lexical context must be
// sent as "kind":"accessor".
// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin-executable %t/mock-plugin-v8#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/output-v8.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-V8 %s < %t/output-v8.txt

// The accessor is the innermost lexical context, so it's the first element.
// CHECK-V7:     "lexicalContext":[{"kind":"declaration",
// CHECK-V7-NOT: "kind":"accessor"

// CHECK-V8: "lexicalContext":[{"kind":"accessor",

//--- test.swift
@freestanding(expression) macro testMacro() -> Int = #externalMacro(module: "TestPlugin", type: "TestMacro")

struct S {
  var x: Int {
    get { #testMacro() }
  }
}

//--- plugin-v7.c
#include "swift-c/MockPlugin/MockPlugin.h"

MOCK_PLUGIN([
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 7}}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestMacro"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "0", "diagnostics": []}}
  }
])

//--- plugin-v8.c
#include "swift-c/MockPlugin/MockPlugin.h"

MOCK_PLUGIN([
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 8}}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestMacro"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "0", "diagnostics": []}}
  }
])
