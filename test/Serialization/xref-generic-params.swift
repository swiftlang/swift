// RUN: %empty-directory(%t)

// Run the same test several times, providing the nested types a different way
// each time.

// RUN: %target-swift-frontend -emit-module -o %t/a.swiftmodule -primary-file %s %S/Inputs/xref-generic-params-other.swift -module-name main
// RUN: %target-swift-frontend -emit-module -o %t/b.swiftmodule %s -primary-file %S/Inputs/xref-generic-params-other.swift -module-name main
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/main.swiftmodule %t/a.swiftmodule %t/b.swiftmodule -module-name main
// RUN: %target-swift-ide-test -print-module -module-to-print=main -I %t -source-filename=x | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/a-extensions.swiftmodule -primary-file %s %S/Inputs/xref-generic-params-other-extensions.swift -module-name extensions
// RUN: %target-swift-frontend -emit-module -o %t/b-extensions.swiftmodule %s -primary-file %S/Inputs/xref-generic-params-other-extensions.swift -module-name extensions
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/extensions.swiftmodule %t/a-extensions.swiftmodule %t/b-extensions.swiftmodule -module-name extensions
// RUN: %target-swift-ide-test -print-module -module-to-print=extensions -I %t -source-filename=x | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/a-extensions_mixed.swiftmodule -primary-file %s %S/Inputs/xref-generic-params-other-extensions-mixed.swift -module-name extensions_mixed
// RUN: %target-swift-frontend -emit-module -o %t/b-extensions_mixed.swiftmodule %s -primary-file %S/Inputs/xref-generic-params-other-extensions-mixed.swift -module-name extensions_mixed
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/extensions_mixed.swiftmodule %t/a-extensions_mixed.swiftmodule %t/b-extensions_mixed.swiftmodule -module-name extensions_mixed
// RUN: %target-swift-ide-test -print-module -module-to-print=extensions_mixed -I %t -source-filename=x | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/a-extensions_constrained.swiftmodule -primary-file %s %S/Inputs/xref-generic-params-other-extensions-constrained.swift -module-name extensions_constrained
// RUN: %target-swift-frontend -emit-module -o %t/b-extensions_constrained.swiftmodule %s -primary-file %S/Inputs/xref-generic-params-other-extensions-constrained.swift -module-name extensions_constrained
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/extensions_constrained.swiftmodule %t/a-extensions_constrained.swiftmodule %t/b-extensions_constrained.swiftmodule -module-name extensions_constrained
// RUN: %target-swift-ide-test -print-module -module-to-print=extensions_constrained -I %t -source-filename=x | %FileCheck %s

public struct A: Equatable {}
public struct B: Equatable {}
public struct C: Equatable {}
public struct D: Equatable {}

// CHECK-LABEL: func test(
public func test(
// CHECK-SAME: _: OuterNonGeneric.InnerNonGeneric.AliasTy
  _: OuterNonGeneric.InnerNonGeneric.AliasTy,
// CHECK-SAME: _: OuterNonGeneric.InnerGeneric<C, D>.AliasTy
  _: OuterNonGeneric.InnerGeneric<C, D>.AliasTy,
// CHECK-SAME: _: OuterGeneric<A, B>.InnerNonGeneric.AliasTy
  _: OuterGeneric<A, B>.InnerNonGeneric.AliasTy,
// CHECK-SAME: _: OuterGeneric<A, B>.InnerGeneric<C, D>.AliasTy
  _: OuterGeneric<A, B>.InnerGeneric<C, D>.AliasTy) {}
