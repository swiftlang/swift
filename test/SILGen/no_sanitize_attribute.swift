// Verify @_noSanitize(<kind>) parses on functions and is preserved through
// SILGen (printed on the AST decl above the SIL function body).

// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

// CHECK-DAG: @_noSanitize(address){{.*}}func noAsan
@_noSanitize(address)
public func noAsan() -> Int { 0 }

// CHECK-DAG: @_noSanitize(thread){{.*}}func noTsan
@_noSanitize(thread)
public func noTsan() -> Int { 1 }

// CHECK-DAG: @_noSanitize(memtag){{.*}}func noMemTag
@_noSanitize(memtag)
public func noMemTag() -> Int { 3 }

// Stacking multiple @_noSanitize attributes on one function is allowed.
// CHECK-DAG: @_noSanitize(address){{.*}}@_noSanitize(thread){{.*}}func stacked
@_noSanitize(address)
@_noSanitize(thread)
public func stacked() -> Int { 4 }

// Also allowed on subscripts.
public struct S {
  // CHECK-DAG: @_noSanitize(address){{.*}}subscript
  @_noSanitize(address)
  public subscript(i: Int) -> Int { i }
}
