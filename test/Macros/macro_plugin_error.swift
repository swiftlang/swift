// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: env MOCKPLUGIN_TESTSPEC="$(cat %t/spec.json)" \
// RUN: %swift-target-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 -enable-experimental-feature Macros \
// RUN:   -load-plugin-executable %swift-mock-plugin#TestPlugin \
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
  // expected-error @-1 {{failedToSendMessage}}
  //FIXME: ^ This should succeed. Error recovery is not implemented.
}

//--- spec.json
[
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
    "response": {"expandFreestandingMacroResult": {"expandedSource": "3", "diagnostics": []}}
  }
]
