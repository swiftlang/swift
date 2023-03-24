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
// RUN:   -swift-version 5 -enable-experimental-feature Macros \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -dump-macro-expansions \
// RUN:   %t/test.swift

//--- test.swift
@freestanding(expression) macro fooMacro(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "FooMacro")

func test() {
  // FIXME: Should be more user friendly message.

  let _: String = #fooMacro(1)
  // expected-error @-1 {{typeMismatch(swiftASTGen.PluginToHostMessage}}
  let _: String = #fooMacro(2)
  // expected-error @-1 {{failedToReceiveMessage}}
  let _: String = #fooMacro(3)
  // ^ This should succeed.
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
                "macro": {"moduleName": "TestPlugin", "typeName": "FooMacro"},
                "syntax": {"kind": "expression", "source": "#fooMacro(1)"}}},
    "response": {"invalidResponse": {}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "TestPlugin", "typeName": "FooMacro"},
                "syntax": {"kind": "expression", "source": "#fooMacro(3)"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "3.description", "diagnostics": []}}
  }
])
