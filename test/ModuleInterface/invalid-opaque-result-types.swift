// Test that we emit a diagnostic (and don't crash) when we cannot resolve
// an opaque result type reference.
//
// First, emit an empty module interface:
//
// RUN: %empty-directory(%t)
// RUN: echo "" | %target-swift-frontend -typecheck -emit-module-interface-path %t/InvalidOpaqueResultType.swiftinterface -enable-library-evolution -swift-version 5 -module-name InvalidOpaqueResultType -
//
// Then, blit some invalid opaque result types into the interface
//
// Test that we reject broken type parameters
// RUN: echo "public typealias SomeGenericBalderdash = @_opaqueReturnTypeOf(\"$somesuchnonsense\", 0) __<InvalidParameter>" >> %t/InvalidOpaqueResultType.swiftinterface
// Test that we reject types we cannot demangle
// RUN: echo "public typealias SomesuchNonsense = @_opaqueReturnTypeOf(\"$somesuchnonsense\", 0) __" >> %t/InvalidOpaqueResultType.swiftinterface
//
// The stage is set:
//
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck %s

// CHECK: unable to resolve type for _opaqueReturnTypeOf attribute
// CHECK: failed to build module 'InvalidOpaqueResultType' for importation
import InvalidOpaqueResultType
