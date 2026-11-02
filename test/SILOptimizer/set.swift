// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -sil-verify-all -module-name=test -Xllvm -sil-disable-pass=function-signature-opts -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -Osize -sil-verify-all -module-name=test -Xllvm -sil-disable-pass=function-signature-opts -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s --check-prefix=CHECK-GLOBAL
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// Test optimal code generation for creating empty sets.

// CHECK-LABEL: sil {{.*}}@$s4test30createEmptySetFromArrayLiteralShySiGyF
// CHECK:         global_addr @_swiftEmptySetSingleton
// CHECK-NOT:     apply
// CHECK:       } // end sil function '$s4test30createEmptySetFromArrayLiteralShySiGyF'
public func createEmptySetFromArrayLiteral() -> Set<Int> {
    return []
}

// CHECK-LABEL: sil {{.*}}@$s4test29createEmptySetWithInitializerShySiGyF
// CHECK:         global_addr @_swiftEmptySetSingleton
// CHECK-NOT:     apply
// CHECK:       } // end sil function '$s4test29createEmptySetWithInitializerShySiGyF'
public func createEmptySetWithInitializer() -> Set<Int> {
    return Set<Int>()
}

// CHECK-LABEL: sil {{.*}}@$s4test17createNonEmptySetShySiGyF
// CHECK:         global_value
// CHECK:         [[F:%[0-9]+]] = function_ref @$sSh21_nonEmptyArrayLiteralShyxGSayxG_tcfCSi_Tt0g5
// CHECK:         apply [[F]]
// CHECK:       } // end sil function '$s4test17createNonEmptySetShySiGyF'
public func createNonEmptySet() -> Set<Int> {
    return [1, 2, 3]
}

public final class Element: Hashable {
  public static func == (lhs: Element, rhs: Element) -> Bool { lhs === rhs }
  public func hash(into hasher: inout Hasher) {}
}

// The global initializer passes the array literal to the set's initializer, so the array's
// semantic calls must not be inlined early.
// CHECK-GLOBAL-LABEL: sil private [global_init_once_fn] @$s4test8emptySet_WZ :
// CHECK-GLOBAL-NOT:     apply
// CHECK-GLOBAL:         global_addr @_swiftEmptySetSingleton
// CHECK-GLOBAL-NOT:     apply
// CHECK-GLOBAL:       } // end sil function '$s4test8emptySet_WZ'
let emptySet: Set<Element> = []

public func getEmptySet() -> Set<Element> {
  emptySet
}
