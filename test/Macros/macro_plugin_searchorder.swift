
// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/src
// RUN: mkdir -p %t/bin
// RUN: mkdir -p %t/lib/tmp
// RUN: mkdir -p %t/lib/plugins
// RUN: mkdir -p %t/external
// RUN: mkdir -p %t/libexec

// RUN: split-file %s %t/src

//#-- For -plugin-path
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/lib/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name MacroDefinition \
// RUN:   -D PLUGIN_PATH \
// RUN:   %t/src/MacroDefinition.swift

//#-- For -load-plugin-library
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/lib/tmp/%target-library-name(MacroDefinition) \
// RUN:   -module-name MacroDefinition \
// RUN:   -D LOAD_PLUGIN_LIBRARY \
// RUN:   %t/src/MacroDefinition.swift

//#-- For -external-plugin-path
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/external/%target-library-name(MacroDefinition) \
// RUN:   -module-name MacroDefinition \
// RUN:   -D EXTERNAL_PLUGIN_PATH \
// RUN:   %t/src/MacroDefinition.swift

//#-- For -load-plugin-executable
// RUN: %swift-build-c-plugin -o %t/libexec/MacroDefinitionPlugin %t/src/MacroDefinition.c

//#-- Expect -load-plugin-library
// RUN: %target-build-swift %t/src/test.swift \
// RUN:   -module-name test \
// RUN:   -load-plugin-library %t/lib/tmp/%target-library-name(MacroDefinition) \
// RUN:   -plugin-path %t/lib/plugins \
// RUN:   -external-plugin-path %t/external#%swift-plugin-server \
// RUN:   -load-plugin-executable  %t/libexec/MacroDefinitionPlugin#MacroDefinition \
// RUN:   -o %t/main1
// RUN: %target-codesign %t/main1
// RUN: %target-run %t/main1 | %FileCheck --check-prefix=CHECK_LOAD_PLUGIN_LIBRARY %s

//#-- Expect -load-plugin-executable
// RUN: %target-build-swift %t/src/test.swift \
// RUN:   -module-name test \
// RUN:   -load-plugin-executable  %t/libexec/MacroDefinitionPlugin#MacroDefinition \
// RUN:   -plugin-path %t/lib/plugins \
// RUN:   -external-plugin-path %t/external#%swift-plugin-server \
// RUN:   -o %t/main2
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck --check-prefix=CHECK_LOAD_PLUGIN_EXECUTABLE %s

//#-- Expect -plugin-path
// RUN: %target-build-swift %t/src/test.swift \
// RUN:   -module-name test \
// RUN:   -plugin-path %t/lib/plugins \
// RUN:   -load-plugin-library %t/lib/tmp/%target-library-name(MacroDefinition) \
// RUN:   -external-plugin-path %t/external#%swift-plugin-server \
// RUN:   -o %t/main3
// RUN: %target-codesign %t/main3
// RUN: %target-run %t/main3 | %FileCheck --check-prefix=CHECK_PLUGIN_PATH %s

//#-- Expect -external-plugin-path
// RUN: %target-build-swift %t/src/test.swift \
// RUN:   -module-name test \
// RUN:   -external-plugin-path %t/external#%swift-plugin-server \
// RUN:   -plugin-path %t/lib/plugins \
// RUN:   -load-plugin-executable  %t/libexec/MacroDefinitionPlugin#MacroDefinition \
// RUN:   -o %t/main4
// RUN: %target-codesign %t/main4
// RUN: %target-run %t/main4 | %FileCheck --check-prefix=CHECK_EXTERNAL_PLUGIN_PATH %s

// CHECK_LOAD_PLUGIN_LIBRARY: load-plugin-library
// CHECK_LOAD_PLUGIN_EXECUTABLE: load-plugin-executable
// CHECK_PLUGIN_PATH: plugin-path
// CHECK_EXTERNAL_PLUGIN_PATH: external-plugin-path

//--- test.swift
@freestanding(expression) macro testMacro() -> String = #externalMacro(module: "MacroDefinition", type: "TestMacro")

print(#testMacro) 

//--- MacroDefinition.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct TestMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
#if PLUGIN_PATH
    return #""plugin-path""#
#elseif LOAD_PLUGIN_LIBRARY
    return #""load-plugin-library""#
#elseif EXTERNAL_PLUGIN_PATH
    return #""external-plugin-path""#
#endif
  }
}

//--- MacroDefinition.c
#include "swift-c/MockPlugin/MockPlugin.h"

MOCK_PLUGIN([
  {
    "expect": {"getCapability": {}},
    "response": {"getCapabilityResult": {"capability": {"protocolVersion": 1}}}
  },
  {
    "expect": {"expandFreestandingMacro": {
                "macro": {"moduleName": "MacroDefinition", "typeName": "TestMacro"},
                "syntax": {"kind": "expression", "source": "#testMacro"}}},
    "response": {"expandFreestandingMacroResult": {"expandedSource": "\"load-plugin-executable\"", "diagnostics": []}}
  }
])
