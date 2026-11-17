// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s --input-file %t.swiftinterface --match-full-lines

// Older compilers don't understand the kind of an OS version query function,
// so the interface falls back to the plain semantics attribute for them.

// CHECK: #if compiler(>=5.3) && $AvailabilityOSVersionQueryKind
// CHECK: @_semantics("availability.osversion(isOSVersionAtLeast)") {{.*}}public func explicitQuery({{.*}}
// CHECK: #else
// CHECK: @_semantics("availability.osversion") {{.*}}public func explicitQuery({{.*}}
// CHECK: #endif
@_semantics("availability.osversion(isOSVersionAtLeast)") @_effects(readnone)
public func explicitQuery(_ a: Int, _ b: Int, _ c: Int) -> Bool { true }

// A plain attribute needs no compatibility guard.
// CHECK-NOT: $AvailabilityOSVersionQueryKind
// CHECK: @_semantics("availability.osversion") {{.*}}public func plainQuery({{.*}}
@_semantics("availability.osversion") @_effects(readnone)
public func plainQuery(_ a: Int, _ b: Int, _ c: Int) -> Bool { true }
