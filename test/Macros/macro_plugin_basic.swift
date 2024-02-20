// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %swift-build-c-plugin -o %t/mock-plugin %t/plugin.c

// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/macro-expansions.txt 2>&1

// RUN: %FileCheck -strict-whitespace %s < %t/macro-expansions.txt

// RUN: not %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -Rmacro-loading \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift \
// RUN:   > %t/macro-loading.txt 2>&1

// RUN: %FileCheck -check-prefix=DIAGS %s < %t/macro-loading.txt

// DIAGS: loaded macro implementation module 'TestPlugin' from executable

// CHECK: ->(plugin:[[#PID:]]) {"getCapability":{"capability":{"protocolVersion":[[#PROTOCOL_VERSION:]]}}}
// CHECK: <-(plugin:[[#PID]]) {"getCapabilityResult":{"capability":{"protocolVersion":1}}}
// CHECK: ->(plugin:[[#PID]]) {"expandFreestandingMacro":{"discriminator":"$s{{.+}}","lexicalContext":[{{.*}}func test{{.*}}],"macro":{"moduleName":"TestPlugin","name":"testString","typeName":"TestStringMacro"},"macroRole":"expression","syntax":{"kind":"expression","location":{"column":19,"fileID":"MyApp/test.swift","fileName":"{{.+}}test.swift","line":5,"offset":301},"source":"#testString(123)"}}}
// CHECK: <-(plugin:[[#PID]]) {"expandFreestandingMacroResult":{"diagnostics":[],"expandedSource":"\"123\"\n  +   \"foo  \""}}
// CHECK: ->(plugin:[[#PID]]) {"expandFreestandingMacro":{"discriminator":"$s{{.+}}","lexicalContext":[{{.*}}],"macro":{"moduleName":"TestPlugin","name":"testStringWithError","typeName":"TestStringWithErrorMacro"},"macroRole":"expression","syntax":{"kind":"expression","location":{"column":19,"fileID":"MyApp/test.swift","fileName":"{{.+}}test.swift","line":6,"offset":336},"source":"#testStringWithError(321)"}}}
// CHECK: <-(plugin:[[#PID]]) {"expandFreestandingMacroResult":{"diagnostics":[{"fixIts":[],"highlights":[],"message":"message from plugin","notes":[],"position":{"fileName":"{{.*}}test.swift","offset":336},"severity":"error"}],"expandedSource":"\"bar\""}}

//--- test.swift
@freestanding(expression) macro testString(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")
@freestanding(expression) macro testStringWithError(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringWithErrorMacro")

func test() {
  let _: String = #testString(123)
  let _: String = #testStringWithError(321)
  // expected-error @-1 {{message from plugin}} 
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
    "response": {"expandFreestandingMacroResult": {"expandedSource": "\"123\"\n  +   \"foo  \"", "diagnostics": []}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestStringWithErrorMacro"},
                "syntax": {"kind": "expression", "source": "#testStringWithError(321)"}}},
    "response": {"expandFreestandingMacroResult": {
                   "expandedSource": "\"bar\"",
                   "diagnostics": [
                     {"severity": "error",
                      "position": {"offset": "=req.expandFreestandingMacro.syntax.location.offset",
                                   "fileName": "=req.expandFreestandingMacro.syntax.location.fileName"},
                      "message":"message from plugin",
                      "highlights": [],
                      "notes": [],
                      "fixIts": []}
                   ]}}
  }
])
