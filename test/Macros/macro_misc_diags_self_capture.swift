// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroPlugin) -module-name=MacroPlugin %t/MacroPlugin.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck -swift-version 5 -load-plugin-library %t/%target-library-name(MacroPlugin) %t/Client.swift -module-name Client -diagnostic-style=llvm 2> %t/diags-5
// RUN: %FileCheck --check-prefix=CHECK-SWIFT5 --implicit-check-not="{{error|warning}}: " -input-file=%t/diags-5 %s

// RUN: not %target-swift-frontend -typecheck -swift-version 6 -load-plugin-library %t/%target-library-name(MacroPlugin) %t/Client.swift -module-name Client -diagnostic-style=llvm 2> %t/diags-6
// RUN: %FileCheck --check-prefix=CHECK-SWIFT6 --implicit-check-not="{{error|warning}}: " -input-file=%t/diags-6 %s

// RUN: %if swift_future %{ not %target-swift-frontend -typecheck -swift-version future -load-plugin-library %t/%target-library-name(MacroPlugin) %t/Client.swift -module-name Client -diagnostic-style=llvm 2> %t/diags-future %}
// RUN: %if swift_future %{ %FileCheck --check-prefix=CHECK-FUTURE --implicit-check-not="{{error|warning}}: " -input-file=%t/diags-future %s %}

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

//--- Client.swift
@freestanding(expression)
macro trailingClosure<T>(_ x: T) -> T = #externalMacro(module: "MacroPlugin", type: "TrailingClosureMacro")

class rdar138997009_Class {
  func foo() {}
  func bar() {
    // rdar://141963700 - This is downgraded to a warning until Swift 6 in the
    // expansion, and until a future language mode for the argument.
    _ = { [self] in
      _ = #trailingClosure {
        foo()
        // CHECK-SWIFT5: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-3]]{{.*}}trailingClosurefMf_.swift:2:9: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this is an error in the Swift 6 language mode
        // CHECK-SWIFT5: Client.swift:[[@LINE-2]]:9: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
        // CHECK-SWIFT6: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-5]]{{.*}}trailingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-SWIFT6: Client.swift:[[@LINE-4]]:9: warning: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit; this will be an error in a future Swift language mode
        // CHECK-FUTURE: @__swiftmacro_6Client0017Clientswift_yEEFcfMX[[@LINE-7]]{{.*}}trailingClosurefMf_.swift:2:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
        // CHECK-FUTURE: Client.swift:[[@LINE-6]]:9: error: call to method 'foo' in closure requires explicit use of 'self' to make capture semantics explicit
      }
    }
  }
}
