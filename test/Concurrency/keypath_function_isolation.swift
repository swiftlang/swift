// RUN: %target-swift-emit-silgen -swift-version 6 -strict-concurrency=complete -enable-actor-data-race-checks -Xllvm -sil-print-types -module-name keypath_function_isolation %s | %FileCheck %s
// RUN: %target-swift-frontend -dump-ast -swift-version 6 -strict-concurrency=complete %s | %FileCheck %s --check-prefix=AST
// RUN: %target-typecheck-verify-swift -swift-version 6 -strict-concurrency=complete -D NEGATIVE

// REQUIRES: concurrency

@MainActor
func sendableKeyPathFunction() -> @Sendable (String) -> Int {
  \.count
}

// CHECK-LABEL: // implicit closure #1 in sendableKeyPathFunction()
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NOT: _checkExpectedExecutor
// CHECK: } // end sil function

// AST-LABEL: func_decl {{.*}} "sendableKeyPathFunction()"
// AST: pattern_named implicit type="any KeyPath<String, Int> & Sendable" "$kp$"
// AST: autoclosure_expr implicit type="@Sendable (String) -> Int"
// AST: declref_expr implicit type="any KeyPath<String, Int> & Sendable" {{.*}}.$kp$

@MainActor
func indexedKeyPathFunction<Index: Hashable & Sendable, Value>(
  _ index: Index
) -> @Sendable ([Index: Value]) -> Value? {
  \.[index]
}

// CHECK-LABEL: // implicit closure #1 in indexedKeyPathFunction
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NOT: _checkExpectedExecutor
// CHECK: } // end sil function

// AST-LABEL: func_decl {{.*}} "indexedKeyPathFunction(_:)"
// AST: pattern_named implicit type="any KeyPath<[Index : Value], Value?> & Sendable" "$kp$"
// AST: autoclosure_expr implicit type="@Sendable ([Index : Value]) -> Value?"

final class NonSendableIndex: Hashable {
  static func == (lhs: NonSendableIndex, rhs: NonSendableIndex) -> Bool {
    lhs === rhs
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }
}

@MainActor
func nonSendableKeyPathFunction(
  _ index: NonSendableIndex
) -> ([NonSendableIndex: Int]) -> Int? {
  \.[index]
}

// CHECK-LABEL: // implicit closure #1 in nonSendableKeyPathFunction
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK: _checkExpectedExecutor
// CHECK: } // end sil function

// AST-LABEL: func_decl {{.*}} "nonSendableKeyPathFunction(_:)"
// AST: pattern_named implicit type="KeyPath<[NonSendableIndex : Int], Int?>" "$kp$"
// AST: autoclosure_expr implicit type="([NonSendableIndex : Int]) -> Int?"

#if NEGATIVE
@MainActor
func invalidSendableKeyPathFunction(
  _ index: NonSendableIndex
) -> @Sendable ([NonSendableIndex: Int]) -> Int? {
  \.[index] // expected-error {{converting non-Sendable function value to '@Sendable ([NonSendableIndex : Int]) -> Int?' may introduce data races}}
}
#endif
