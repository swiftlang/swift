// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugin)
// RUN: %empty-directory(%t/external-plugin)
// RUN: %empty-directory(%t/lib)
// RUN: %empty-directory(%t/src)

// RUN: split-file %s %t/src

//#-- Prepare the macro shared library plugin for -plugin-path.
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/plugin/%target-library-name(StringifyPlugin) \
// RUN:   -module-name StringifyPlugin \
// RUN:   %t/src/StringifyPlugin.swift

//#-- Prepare the macro shared library plugin for -external-plugin-path.
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library -o %t/external-plugin/%target-library-name(AssertPlugin) \
// RUN:   -module-name AssertPlugin \
// RUN:   %t/src/AssertPlugin.swift

//#-- Prepare the macro executable plugin for -load-plugin-executable.
// RUN: %swift-build-c-plugin -o %t/mock-plugin %t/src/plugin.c

//#-- Prepare the macro library.
// RUN: %target-swift-frontend \
// RUN:   -swift-version 5 \
// RUN:   -emit-module -o %t/lib/MacroLib.swiftmodule \
// RUN:   -module-name MacroLib \
// RUN:   -plugin-path %t/plugin \
// RUN:   -external-plugin-path %t/external-plugin#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -primary-file %t/src/macro_library.swift \
// RUN:   -emit-reference-dependencies-path %t/macro_library.swiftdeps \
// RUN:   -emit-loaded-module-trace-path %t/macro_library.trace.json \
// RUN:   -emit-dependencies-path %t/macro_library.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/macro_library.swiftdeps > %t/macro_library.swiftdeps.processed
// RUN: %FileCheck --check-prefix WITH_PLUGIN %s < %t/macro_library.swiftdeps.processed
// RUN: %FileCheck --check-prefix TRACE_WITH_PLUGIN %s < %t/macro_library.trace.json
// RUN: %FileCheck --check-prefix D_WITH_PLUGIN %s < %t/macro_library.d

//#-- Without macro (no -D USE_MACRO)
// RUN: %target-swift-frontend \
// RUN:   -swift-version 5 -typecheck \
// RUN:   -primary-file %t/src/test.swift \
// RUN:   %t/src/other.swift \
// RUN:   -I %t/lib \
// RUN:   -plugin-path %t/plugin \
// RUN:   -external-plugin-path %t/external-plugin#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -emit-reference-dependencies-path %t/without_macro.swiftdeps \
// RUN:   -emit-loaded-module-trace-path %t/without_macro.trace.json \
// RUN:   -emit-dependencies-path %t/without_macro.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/without_macro.swiftdeps > %t/without_macro.swiftdeps.processed
// RUN: %FileCheck --check-prefix WITHOUT_PLUGIN %s < %t/without_macro.swiftdeps.processed
// RUN: %FileCheck --check-prefix TRACE_WITHOUT_PLUGIN %s < %t/without_macro.trace.json
// RUN: %FileCheck --check-prefix D_WITHOUT_PLUGIN %s < %t/without_macro.d

//#-- With macro - primary (-D USE_MACRO)
// RUN: %target-swift-frontend \
// RUN:   -D USE_MACRO \
// RUN:   -swift-version 5 -typecheck \
// RUN:   -primary-file %t/src/test.swift \
// RUN:   %t/src/other.swift \
// RUN:   -I %t/lib \
// RUN:   -plugin-path %t/plugin \
// RUN:   -external-plugin-path %t/external-plugin#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -emit-reference-dependencies-path %t/with_macro_primary.swiftdeps \
// RUN:   -emit-loaded-module-trace-path %t/with_macro_primary.trace.json \
// RUN:   -emit-dependencies-path %t/with_macro_primary.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/with_macro_primary.swiftdeps > %t/with_macro_primary.swiftdeps.processed
// RUN: %FileCheck --check-prefix WITH_PLUGIN %s < %t/with_macro_primary.swiftdeps.processed
// RUN: %FileCheck --check-prefix TRACE_WITH_PLUGIN %s < %t/with_macro_primary.trace.json
// RUN: %FileCheck --check-prefix D_WITH_PLUGIN %s < %t/with_macro_primary.d

//#-- With macro - non-primary (-D USE_MACRO)
// RUN: %target-swift-frontend \
// RUN:   -D USE_MACRO \
// RUN:   -swift-version 5 -typecheck \
// RUN:   %t/src/test.swift \
// RUN:   -primary-file %t/src/other.swift \
// RUN:   -I %t/lib \
// RUN:   -plugin-path %t/plugin \
// RUN:   -external-plugin-path %t/external-plugin#%swift-plugin-server \
// RUN:   -load-plugin-executable %t/mock-plugin#TestPlugin \
// RUN:   -emit-reference-dependencies-path %t/with_macro_nonprimary.swiftdeps \
// RUN:   -emit-loaded-module-trace-path %t/with_macro_nonprimary.trace.json \
// RUN:   -emit-dependencies-path %t/with_macro_nonprimary.d
// RUN: %{python} %S/../Inputs/process_fine_grained_swiftdeps.py %swift-dependency-tool %t/with_macro_nonprimary.swiftdeps > %t/with_macro_nonprimary.swiftdeps.processed
// RUN: %FileCheck --check-prefix WITHOUT_PLUGIN %s < %t/with_macro_nonprimary.swiftdeps.processed
// RUN: %FileCheck --check-prefix TRACE_WITHOUT_PLUGIN %s < %t/with_macro_nonprimary.trace.json
// RUN: %FileCheck --check-prefix D_WITHOUT_PLUGIN %s < %t/with_macro_nonprimary.d

// WITH_PLUGIN-DAG: externalDepend interface '' '{{.*}}mock-plugin' false
// WITH_PLUGIN-DAG: externalDepend interface '' '{{.*}}StringifyPlugin.{{(dylib|so|dll)}}' false
// WITH_PLUGIN-DAG: externalDepend interface '' '{{.*}}AssertPlugin.{{(dylib|so|dll)}}' false

// WITHOUT_PLUGIN-NOT:  StringifyPlugin
// WITHOUT_PLUGIN-NOT:  AssertPlugin
// WITHOUT_PLUGIN-NOT:  mock-plugin

// TRACE_WITH_PLGUIN: "swiftmacros":[
// TRACE_WITH_PLUGIN-DAG: {"name":"AssertPlugin","path":"{{.*}}AssertPlugin.{{(dylib|so|dll)}}"}
// TRACE_WITH_PLUGIN-DAG: {"name":"StringifyPlugin","path":"{{.*}}StringifyPlugin.{{(dylib|so|dll)}}"}
// TRACE_WITH_PLUGIN-DAG: {"name":"TestPlugin","path":"{{.*}}mock-plugin"}
// TRACE_WITH_PLUGIN: ]

// TRACE_WITHOUT_PLUGIN-NOT:  StringifyPlugin
// TRACE_WITHOUT_PLUGIN-NOT:  AssertPlugin
// TRACE_WITHOUT_PLUGIN-NOT:  mock-plugin
// TRACE_WITHOUT_PLUGIN: "swiftmacros":[]

// D_WITH_PLUGIN-DAG: AssertPlugin.{{(dylib|so|dll)}}
// D_WITH_PLUGIN-DAG: StringifyPlugin.{{(dylib|so|dll)}}
// D_WITH_PLUGIN-DAG: mock-plugin

// D_WITHOUT_PLUGIN-NOT:  StringifyPlugin
// D_WITHOUT_PLUGIN-NOT:  AssertPlugin
// D_WITHOUT_PLUGIN-NOT:  mock-plugin

//--- macro_library.swift
@freestanding(expression) public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "StringifyPlugin", type: "StringifyMacro")
@freestanding(expression) public macro assert(_ value: Bool) = #externalMacro(module: "AssertPlugin", type: "AssertMacro")
@freestanding(expression) public macro testString(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "TestStringMacro")

public func funcInMacroLib() {}

//--- test.swift
import MacroLib

func test(a: Int, b: Int) {
  // Just using MacroLib without macro
  funcInMacroLib()

#if USE_MACRO
  _ = #stringify(a + b)
  _ = #testString(123)
  #assert(true)
#endif
}

//--- other.swift
import MacroLib

func test() {
  // Just using MacroLib without macro
  funcInMacroLib()
}

//--- StringifyPlugin.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct StringifyMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.arguments.first?.expression else {
      fatalError("boom")
    }

    return "(\(argument), \(StringLiteralExprSyntax(content: argument.description)))"
  }
}

//--- AssertPlugin.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct AssertMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.arguments.first?.expression else {
      fatalError("boom")
    }

    return "assert(\(argument))"
  }
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
    "response": {"expandFreestandingMacroResult": {"expandedSource": "\"test\"", "diagnostics": []}}
  }
])
