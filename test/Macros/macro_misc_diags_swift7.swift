// REQUIRES: swift_swift_parser
// REQUIRES: swift7

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// RUN: not %target-swift-frontend -typecheck -swift-version 7 -load-plugin-library %t/%target-library-name(MacroPlugin) %t/Client.swift -module-name Client -diagnostic-style=llvm 2> %t/diags
// RUN: %FileCheck --check-prefix=CHECK-DIAG --implicit-check-not="{{error|warning}}: " -input-file=%t/diags %s

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct TrailingClosureMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.trailingClosure else {
      fatalError()
    }
    return "\(argument)"
  }
}

public struct MakeFunc : DeclarationMacro {
  static public func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    ["func expansionFn() -> Int { 0 }"]
  }
}

//--- Client.swift
@freestanding(expression)
macro trailingClosure<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "TrailingClosureMacro")

@freestanding(declaration, names: named(expansionFn))
macro makeFunc<T>(_ x: T) = #externalMacro(module: "MacroPlugin", type: "MakeFunc")

class rdar138997009_Class {
  func foo() {}
  func bar() {
    // rdar://141963700 - In Swift 7 these are errors.
    _ = {
      _ = #trailingClosure {
        foo()
        // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-DIAG: Client.swift:[[@LINE-2]]:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
      }
      // Use an attribute to force a MacroExpansionDecl (otherwise we parse a
      // MacroExpansionExpr)
      @discardableResult
      #makeFunc(foo())
      // CHECK-DIAG: Client.swift:[[@LINE-1]]:17: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
    }
  }
}
