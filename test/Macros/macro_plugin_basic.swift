// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %clang \
// RUN:  -isysroot %sdk \
// RUN:  -I %swift_src_root/include \
// RUN:  -L %swift-lib-dir -l_swiftMockPlugin \
// RUN:  -Wl,-rpath,%swift-lib-dir \
// RUN:  -o %t/mock-plugin \
// RUN:  %t/plugin.c

// RUN: %swift-target-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -dump-macro-expansions \
// RUN:   %t/test.swift \
// RUN:   2>&1 | tee %t/macro-expansions.txt

// RUN: %FileCheck -strict-whitespace %s < %t/macro-expansions.txt

//--- test.swift
@freestanding(expression) macro testString(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")
@freestanding(expression) macro testStringWithError(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringWithErrorMacro")

func test() {
  let _: String = #testString(123)
  let _: String = #testStringWithError(321)
  // expected-error @-1 {{message from plugin}} 
}

// CHECK:      ------------------------------
// CHECK-NEXT: {{^}}"123"
// CHECK-NEXT: {{^}}  +   "foo  "
// CHECK-NEXT: ------------------------------

// CHECK:      ------------------------------
// CHECK-NEXT: {{^}}"bar"
// CHECK-NEXT: ------------------------------

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
