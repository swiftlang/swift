// Verify @noSanitize(<kind>) parses on functions and is preserved through
// SILGen (printed on the AST decl above the SIL function body).

// REQUIRES: swift_feature_NoSanitize

// RUN: %target-swift-emit-silgen -parse-as-library \
// RUN:   -enable-experimental-feature NoSanitize %s | %FileCheck %s

// CHECK-DAG: @noSanitize(address){{.*}}func noAsan
@noSanitize(address)
public func noAsan() -> Int { 0 }

// CHECK-DAG: @noSanitize(thread){{.*}}func noTsan
@noSanitize(thread)
public func noTsan() -> Int { 1 }

// CHECK-DAG: @noSanitize(memtag){{.*}}func noMemTag
@noSanitize(memtag)
public func noMemTag() -> Int { 3 }

// Stacking multiple @noSanitize attributes on one function is allowed.
// CHECK-DAG: @noSanitize(address){{.*}}@noSanitize(thread){{.*}}func stacked
@noSanitize(address)
@noSanitize(thread)
public func stacked() -> Int { 4 }

// Also allowed on subscripts.
public struct S {
  // CHECK-DAG: @noSanitize(address){{.*}}subscript
  @noSanitize(address)
  public subscript(i: Int) -> Int { i }
}
