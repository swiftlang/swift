// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: sed -i '' -e 's#UTILS_DIR#%utils#' %t/plugin
// RUN: sed -i '' -e 's#TEMP_DIR#%t#' %t/plugin
// RUN: chmod +x %t/plugin

// RUN: %swift-target-frontend -typecheck -verify -swift-version 5 -enable-experimental-feature Macros -load-plugin-executable %t/plugin#TestPlugin %t/test.swift

//--- test.swift
@freestanding(expression) macro testString(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")
@freestanding(expression) macro testStringWithError(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringWithErrorMacro")

func test() {
  let _: String = #testString(123)
  let _: String = #testStringWithError(321)
  // expected-error @-1 {{message from plugin}} 
}

//--- plugin
#!/usr/bin/env python3
import sys
sys.path.append('UTILS_DIR')

import mock_plugin

mock_plugin.TEST_SPEC = [
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 1}}},
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestStringMacro"},
                "syntax": {"kind": "expression", "source": "#testString(123)"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "\"123\"", "diagnostics": []}},
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "TestStringWithErrorMacro"},
                "syntax": {"kind": "expression", "source": "#testStringWithError(321)"}}},
    "response": {"expandFreestandingMacroResult": {
                   "expandedSource": "\"123\"",
                   "diagnostics": [
                     {"severity": "error",
                      "position": {"offset": "={req[expandFreestandingMacro][syntax][location][offset]}",
                                   "fileName": "{req[expandFreestandingMacro][syntax][location][fileName]}"},
                      "message":"message from plugin",
                      "highlights": [],
                      "notes": [],
                      "fixIts": []}
                   ]}},
  },
]

mock_plugin.main()

