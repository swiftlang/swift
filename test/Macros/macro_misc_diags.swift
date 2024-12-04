// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// RUN: not %target-swift-frontend -typecheck -swift-version 6 -load-plugin-library %t/%target-library-name(MacroPlugin) %t/Client.swift -module-name Client -diagnostic-style=llvm 2> %t/diags
// RUN: %FileCheck --check-prefix=CHECK-DIAG --implicit-check-not="{{error|warning}}: " -input-file=%t/diags %s

//--- MacroPlugin.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct IdentityMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.arguments.first else {
      fatalError()
    }
    return "\(argument)"
  }
}

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

public struct MakeBinding : DeclarationMacro {
  static public func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard let arg = node.arguments.first else {
      fatalError()
    }
    return ["let x = \(arg)"]
  }
}

//--- Client.swift
@freestanding(expression)
macro identity<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "IdentityMacro")

@freestanding(expression)
macro trailingClosure<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "TrailingClosureMacro")

@freestanding(declaration, names: named(x))
macro makeBinding<T>(_ x: T) = #externalMacro(module: "MacroPlugin", type: "MakeBinding")

@available(*, deprecated)
func deprecatedFunc() -> Int { 0 }

// Make sure we do MiscDiagnostics passes for both macro arguments and expansions.

_ = #identity(Int)
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-2]]{{.*}}identityfMf_.swift:1:1: error: expected member name or initializer call after type name
// CHECK-DIAG: Client.swift:[[@LINE-2]]:15: error: expected member name or initializer call after type name

_ = {
  _ = #identity(Int)
  // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-2]]{{.*}}identityfMf0_.swift:1:1: error: expected member name or initializer call after type name
  // CHECK-DIAG: Client.swift:[[@LINE-2]]:17: error: expected member name or initializer call after type name
}

_ = #identity(deprecatedFunc())
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-2]]{{.*}}identityfMf1_.swift:1:1: warning: 'deprecatedFunc()' is deprecated
// CHECK-DIAG: Client.swift:[[@LINE-2]]:15: warning: 'deprecatedFunc()' is deprecated

#makeBinding((deprecatedFunc(), Int, {
  if let _ = takesClosure {} {}
}()))
// CHECK-DIAG: Client.swift:[[@LINE-3]]:33: error: expected member name or initializer call after type name
// CHECK-DIAG: Client.swift:[[@LINE-4]]:15: warning: 'deprecatedFunc()' is deprecated
// CHECK-DIAG: Client.swift:[[@LINE-4]]:27: warning: trailing closure in this context is confusable with the body of the statement
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-7]]{{.*}}makeBindingfMf_.swift:1:28: error: expected member name or initializer call after type name
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-8]]{{.*}}makeBindingfMf_.swift:1:10: warning: 'deprecatedFunc()' is deprecated
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-9]]{{.*}}makeBindingfMf_.swift:2:27: warning: trailing closure in this context is confusable with the body of the statement
// CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-10]]{{.*}}makeBindingfMf_.swift:1:5: warning: initialization of immutable value

struct S1 {
  #makeBinding(deprecatedFunc())
  // CHECK-DIAG: Client.swift:[[@LINE-1]]:16: warning: 'deprecatedFunc()' is deprecated
  // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}makeBindingfMf_.swift:1:9: warning: 'deprecatedFunc()' is deprecated
}

struct S2 {
  #makeBinding({deprecatedFunc()})
  // CHECK-DIAG: Client.swift:[[@LINE-1]]:17: warning: 'deprecatedFunc()' is deprecated
  // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}makeBindingfMf_.swift:2:5: warning: 'deprecatedFunc()' is deprecated
}

func takesClosure(_ fn: () -> Void) -> Int? { nil }

_ = #trailingClosure {
  if let _ = takesClosure {} {}
  // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:27: warning: trailing closure in this context is confusable with the body of the statement
  // CHECK-DIAG: Client.swift:[[@LINE-2]]:27: warning: trailing closure in this context is confusable with the body of the statement
}

func testOptionalToAny(_ y: Int?) {
  _ = #trailingClosure {
    let _: Any = y
    // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:18: warning: expression implicitly coerced from 'Int?' to 'Any'
    // CHECK-DIAG: Client.swift:[[@LINE-2]]:18: warning: expression implicitly coerced from 'Int?' to 'Any'
  }
}

// rdar://138997009 - Make sure we don't crash in MiscDiagnostics' implicit
// self diagnosis.
struct rdar138997009 {
  func foo() {}
  func bar() {
    _ = {
      _ = #trailingClosure {
        foo()
      }
    }
  }
}

class rdar138997009_Class {
  func foo() {}
  func bar() {
    _ = {
      _ = #trailingClosure {
        foo()
        // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-DIAG: Client.swift:[[@LINE-2]]:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
      }
    }
  }
}
