// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: split-file --leading-lines %s %t

// Check that indexing and cursor info both point to the expansion site rather
// than generated macro buffer.

// Create a plugin that adds a new function as a member
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// Prepare a test module that uses the macro in a struct
// RUN: %target-swift-frontend -emit-module -emit-module-source-info -module-name TestModule -o %t/mods -load-plugin-library %t/%target-library-name(MacroPlugin) %t/TestModule.swift -index-store-path %t/idx

// Check we correctly output the added `newFunc` on the line of the attached
// macro.
// RUN: c-index-test core -print-record %t/idx | %FileCheck %s --check-prefix=INDEX

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

public struct AddNamedFuncMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    let newFunc: DeclSyntax =
      """
      public func newFunc() {}
      """
    return [
      newFunc,
    ]
  }
}

//--- TestModule.swift
@attached(
  member,
  names: named(newFunc)
)
public macro AddNamedFunc() = #externalMacro(module: "MacroPlugin", type: "AddNamedFuncMacro")

// INDEX: [[@LINE+2]]:1 | instance-method/Swift | s:10TestModule9ModStructV7newFuncyyF
// INDEX-SAME: Def,Impl
@AddNamedFunc
// MOD_CURSOR: source.lang.swift.ref.function.method.instance
// MOD_CURSOR-SAME: TestModule.swift:[[@LINE-2]]:1
// MOD_CURSOR: newFunc()
public struct ModStruct {
  public func existingFunc() {}
}

//--- test.swift
import TestModule

@AddNamedFunc
// LOCAL_CURSOR: source.lang.swift.ref.function.method.instance
// LOCAL_CURSOR-SAME: [[@LINE-2]]:1
// LOCAL_CURSOR: newFunc()
public struct LocalStruct {
  public func existingFunc() {}
}

// Check the location in cursor info is the attached macro.
func test(l: LocalStruct, m: ModStruct) {
  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/test.swift -- -I %t/mods -load-plugin-library %t/%target-library-name(MacroPlugin) -target %target-triple %t/test.swift | %FileCheck %s --check-prefix=LOCAL_CURSOR
  l.newFunc()

  // RUN: %sourcekitd-test -req=cursor -pos=%(line+1):5 %t/test.swift -- -I %t/mods -target %target-triple %t/test.swift | %FileCheck %s --check-prefix=MOD_CURSOR
  m.newFunc()
}
