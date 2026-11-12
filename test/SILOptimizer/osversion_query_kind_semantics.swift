// RUN: %target-swift-frontend -emit-sil -O -module-name h %s
// RUN: %target-swift-frontend -emit-silgen -module-name h %s | %FileCheck %s

// A function that carries `availability.osversion` without a kind that can be
// inferred is not an OS version query, and the optimizer leaves it alone.
// CHECK: sil {{.*}}[_semantics "availability.osversion"] {{.*}}@$s1h9fakeQueryySbSi_S2itF
@_semantics("availability.osversion") @_effects(readnone) @inline(never)
public func fakeQuery(_ a: Int, _ b: Int, _ c: Int) -> Bool { true }

public func caller() -> Bool { fakeQuery(1, 0, 0) }

// An explicit kind is kept as written.
// CHECK: sil {{.*}}[_semantics "availability.osversion(isOSVersionAtLeast)"] {{.*}}@$s1h13explicitQueryySbSi_S2itF
@_semantics("availability.osversion(isOSVersionAtLeast)") @_effects(readnone) @inline(never)
public func explicitQuery(_ a: Int, _ b: Int, _ c: Int) -> Bool { true }

public func caller2() -> Bool { explicitQuery(1, 0, 0) }
