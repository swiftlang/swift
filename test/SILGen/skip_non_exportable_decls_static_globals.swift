// RUN: %empty-directory(%t)

// Globals that must be initialized statically still get their initializer
// emitted under -experimental-skip-non-exportable-decls, as long as they are
// exposed to clients. Otherwise no static initializer can be formed and the
// mandatory diagnostics reject the module.

// RUN: %target-swift-frontend -emit-module %s -parse-as-library \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -enable-experimental-feature CompileTimeValues \
// RUN:   -experimental-skip-non-exportable-decls \
// RUN:   -emit-module-path %t/Test.swiftmodule

// RUN: %target-swift-frontend -emit-module %s -parse-as-library \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -enable-experimental-feature CompileTimeValues \
// RUN:   -experimental-skip-non-exportable-decls \
// RUN:   -experimental-lazy-typecheck \
// RUN:   -experimental-skip-non-inlinable-function-bodies \
// RUN:   -emit-module-path %t/TestLazy.swiftmodule

// RUN: %target-swift-frontend -emit-sil %s -parse-as-library \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -enable-experimental-feature CompileTimeValues \
// RUN:   -experimental-skip-non-exportable-decls | %FileCheck %s

// A global that is not initialized with a compile-time value is still
// diagnosed.
// RUN: %target-swift-frontend -emit-module %s -parse-as-library \
// RUN:   -enable-library-evolution -module-name Test \
// RUN:   -enable-experimental-feature CompileTimeValues \
// RUN:   -experimental-skip-non-exportable-decls \
// RUN:   -emit-module-path %t/TestBad.swiftmodule -DBAD -verify

// REQUIRES: swift_feature_CompileTimeValues

// CHECK-LABEL: sil_global private [let] [used] [section "__DATA,__tuple"] @$s4Test10tupleValues5UInt8V_A2Dtvp : $(UInt8, UInt8, UInt8) = {
@section("__DATA,__tuple") @used
@usableFromInline internal let tupleValue: (UInt8, UInt8, UInt8) = (1, 2, 3)

// CHECK-LABEL: sil_global private [let] [section "__DATA,__public"] @$s4Test11publicValueSivp : $Int = {
@section("__DATA,__public")
public let publicValue: Int = 1

// CHECK-LABEL: sil_global private [let] @$s4Test10constValueSivp : $Int = {
@const public let constValue: Int = 2

// CHECK-LABEL: sil_global private [let] @$s4Test16constInitializedSivp : $Int = {
@constInitialized public let constInitialized: Int = 3

// CHECK-LABEL: sil_global private [let] @my_silgen_name_global : $Int = {
@_silgen_name("my_silgen_name_global")
public let silgenNameValue: Int = 4

public struct S {
  // CHECK-LABEL: sil_global private [let] [section "__DATA,__static"] @$s4Test1SV11staticValueSivpZ : $Int = {
  @section("__DATA,__static")
  public static let staticValue: Int = 5
}

// Globals that are not exposed to clients are still skipped entirely.
// CHECK-NOT: internalValue
@section("__DATA,__internal")
internal let internalValue: Int = 6

#if BAD
@section("__DATA,__bad")
public let badValue: [Int] = [1, 2, 3]
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
#endif
