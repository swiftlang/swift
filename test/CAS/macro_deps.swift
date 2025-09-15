// REQUIRES: swift_swift_parser

/// Test loading dependencies that has macros.
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Build macros.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroOne) -module-name=MacroOne %t/macro-1.swift
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroTwo) -module-name=MacroTwo %t/macro-2.swift

/// Build binary module that depends on textual module that uses macro.
// RUN: %target-swift-frontend -emit-module -module-cache-path %t/clang-module-cache %t/test.swift -module-name Test -o %t/include/Test.swiftmodule -I %t/include \
// RUN:   -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -swift-version 5 -external-plugin-path %t#%swift-plugin-server

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -I %t/include \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -external-plugin-path %t#%swift-plugin-server

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Foo macroDependencies | %FileCheck %s --check-prefix=FOO-DEPS
// FOO-DEPS: MacroOne
// FOO-DEPS-NOT: MacroTwo

// RR: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Baz macroDependencies | %FileCheck %s --check-prefix=BAZ-DEPS
// BAZ-DEPS: MacroOne

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json MyApp macroDependencies | %FileCheck %s --check-prefix=APP-DEPS
// APP-DEPS: MacroOne
// APP-DEPS: MacroTwo

/// Build all dependencies.
// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/SwiftShims.cmd
// RUN: %swift_frontend_plain @%t/SwiftShims.cmd
// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json Foo > %t/Foo.cmd
// RUN: %swift_frontend_plain @%t/Foo.cmd
// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json Bar > %t/Bar.cmd
// RUN: %swift_frontend_plain @%t/Bar.cmd
// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json Baz > %t/Baz.cmd
// RUN: %swift_frontend_plain @%t/Baz.cmd

// RUN: %FileCheck %s --check-prefix=PLUGIN_SEARCH --input-file=%t/Bar.cmd
// PLUGIN_SEARCH-NOT: -external-plugin-path

// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json MyApp > %t/MyApp.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %FileCheck %s --check-prefix=PLUGIN_SEARCH --check-prefix=RESOLVED --input-file=%t/MyApp.cmd
// PLUGIN_SEARCH-NOT: -external-plugin-path
// RESOLVED-COUNT-2: -load-resolved-plugin

// RUN: %target-swift-frontend -diagnostic-style=swift \
// RUN:   -emit-module -o %t/Test.swiftmodule -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -external-plugin-path %t#%swift-plugin-server \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name MyApp -explicit-swift-module-map-file @%t/map.casid -O \
// RUN:   %t/main.swift @%t/MyApp.cmd

//--- macro-1.swift
import SwiftSyntax
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

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

//--- macro-2.swift
import SwiftSyntax
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros

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

//--- include/Foo.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -swift-version 5 -O -module-name Foo -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
import Swift
public func foo()
@freestanding(expression) public macro assert(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")
@inlinable
public func assertFalse() {
    #assert(false)
}

//--- include/Bar.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -swift-version 5 -O -module-name Bar -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
public func bar()

//--- include/Baz.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -enable-library-evolution -swift-version 5 -O -module-name Baz -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib
import Foo
// have access to #assert here

//--- test.swift
import Foo
@inlinable
public func test() {
    #assert(true)
}
@freestanding(expression) public macro assertTest(_: Bool) = #externalMacro(module: "MacroOne", type: "AssertMacro")

//--- main.swift
import Test
import Bar
import Baz
@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroTwo", type: "StringifyMacro")

func appTest() {
    let str = #stringify("test")
    test()
    #assertTest(true)
}
