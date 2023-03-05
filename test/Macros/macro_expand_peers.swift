// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -I %swift-host-lib-dir -disable-availability-checking
// RUN: %target-swift-frontend -swift-version 5 -typecheck -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -I %swift-host-lib-dir %s -disable-availability-checking -dump-macro-expansions > %t/expansions-dump.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-DUMP %s < %t/expansions-dump.txt

// RUN: %target-build-swift -swift-version 5 -Xfrontend -disable-availability-checking -load-plugin-library %t/%target-library-name(MacroDefinition) -parse-as-library -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser
// RUN: %target-run %t/main | %FileCheck %s -check-prefix=CHECK-EXEC

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(peer, names: overloaded)
macro addCompletionHandler() = #externalMacro(module: "MacroDefinition", type: "AddCompletionHandler")

struct S {
  @addCompletionHandler
  func f(a: Int, for b: String, _ value: Double) async -> String {
    return b
  }

  // CHECK-DUMP: @__swiftmacro_18macro_expand_peers1SV1f1a3for_SSSi_SSSdtYaF20addCompletionHandlerfMp_.swift
  // CHECK-DUMP: func f(a: Int, for b: String, _ value: Double, completionHandler: @escaping (String) -> Void) {
  // CHECK-DUMP:   Task {
  // CHECK-DUMP:     completionHandler(await f(a: a, for: b, value))
  // CHECK-DUMP:   }
  // CHECK-DUMP: }

  func useOverload(_ body: @escaping (String) -> Void) {
    self.f(a: 1, for: "hahaha local", 2.0) {
      body($0)
    }
  }
}

// CHECK-DUMP: @__swiftmacro_18macro_expand_peers1f1a3for_SSSi_SSSdtYaF20addCompletionHandlerfMp_.swift
// CHECK-DUMP: func f(a: Int, for b: String, _ value: Double, completionHandler: @escaping (String) -> Void) {
// CHECK-DUMP:   Task {
// CHECK-DUMP:     completionHandler(await f(a: a, for: b, value))
// CHECK-DUMP:   }
// CHECK-DUMP: }

@addCompletionHandler
func f(a: Int, for b: String, _ value: Double) async -> String {
  return b
}

func useOverload(_ body: @escaping (String) -> Void) {
  f(a: 1, for: "hahaha global", 2.0) {
    body($0)
  }
}

@attached(peer)
macro wrapInType() = #externalMacro(module: "MacroDefinition", type: "WrapInType")

@wrapInType
func global(a: Int, b: String) {
  print(a, b)
}

// CHECK-DUMP: @__swiftmacro_18macro_expand_peers6global1a1bySi_SStF10wrapInTypefMp_.swift
// CHECK-DUMP: struct $s18macro_expand_peers6global1a1bySi_SStF10wrapInTypefMp_6globalfMu0_ {
// CHECK-DUMP:   func $s18macro_expand_peers6global1a1bySi_SStF10wrapInTypefMp_6globalfMu_(a: Int, b: String)  {
// CHECK-DUMP:     global(a: a, b: b)
// CHECK-DUMP:   }
// CHECK-DUMP: }

@main
struct Main {
  static func main() async {
    let result1 = await withCheckedContinuation { cont in
      S().useOverload {
        cont.resume(returning: $0)
      }
    }
    print(result1)
    // CHECK-EXEC: hahaha local
    let result2 = await withCheckedContinuation { cont in
      useOverload {
        cont.resume(returning: $0)
      }
    }
    print(result2)
    // CHECK-EXEC: hahaha global
  }
}
