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

public struct CallClosureMacro: ExpressionMacro {
  public static func expansion(
    of macro: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) -> ExprSyntax {
    guard let argument = macro.trailingClosure else {
      fatalError()
    }
    return "\(argument)()"
  }
}

public struct AddFunctionThatCallsClosureMacro: PeerMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingPeersOf declaration: some DeclSyntaxProtocol,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    guard case .argumentList(let args) = node.arguments else {
      fatalError()
    }
    let arg = args.first!
    return ["func qux() { \(arg)() }"]
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
macro identity<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "IdentityMacro")

@freestanding(expression)
macro trailingClosure<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "TrailingClosureMacro")

@freestanding(expression)
macro takesNonEscapingClosure(_ x: () -> Void) = #externalMacro(module: "MacroPlugin", type: "CallClosureMacro")

@attached(peer, names: named(qux))
macro AddFunctionThatCallsClosure<T>(_ fn: () -> T) = #externalMacro(module: "MacroPlugin", type: "AddFunctionThatCallsClosureMacro")

@freestanding(declaration, names: named(x))
macro makeBinding<T>(_ x: T) = #externalMacro(module: "MacroPlugin", type: "MakeBinding")

@freestanding(declaration, names: named(expansionFn))
macro makeFunc<T>(_ x: T) = #externalMacro(module: "MacroPlugin", type: "MakeFunc")

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
    // rdar://141963700 - Downgrade these to a warning for the macro argument,
    // but is still an error in the expansion.
    _ = {
      _ = #trailingClosure {
        foo()
        // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-DIAG: Client.swift:[[@LINE-2]]:9: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
      }
      // Use an attribute to force a MacroExpansionDecl (otherwise we parse a
      // MacroExpansionExpr)
      @discardableResult
      #makeFunc(foo())
      // CHECK-DIAG: Client.swift:[[@LINE-1]]:17: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
    }
  }
}

// https://github.com/swiftlang/swift/issues/80561
class TestNonEscaping {
  func foo() {}
  func bar() {
    _ = #takesNonEscapingClosure {
      foo()
    }
    _ = {
      _ = #takesNonEscapingClosure {
        foo()
        // CHECK-DIAG: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}takesNonEscapingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-DIAG: Client.swift:[[@LINE-2]]:9: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
      }
    }

    @AddFunctionThatCallsClosure({ foo() })
    func baz() {}
  }
  func qux() {
    @AddFunctionThatCallsClosure({ _ = { foo() } })
    func baz() {}
    // CHECK-DIAG: Client.swift:[[@LINE-2]]:42: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
    // CHECK-DIAG: @__swiftmacro_6Client15TestNonEscapingC3quxyyF7baz_$l{{.*}}AddFunctionThatCallsClosurefMp_.swift:4:13: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
  }
}
