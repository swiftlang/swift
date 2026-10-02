// Regression test for a typed-throws IRGen crash on targets without a
// swifterror register (e.g. PowerPC64). When cross-module optimization
// specializes a call so that the caller's error-result slot holds a typed
// error's concrete (non-pointer aggregate) storage type, IRGen used to feed
// that aggregate to icmp/ptrtoint/PHI and to zero-initialize it with a
// null-of-aggregate constant, producing invalid IR / an instruction-selection
// crash. Compiling for PowerPC64 must succeed and emit a scalar has-error test.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -O -enable-default-cmo -module-name Other -emit-module-path %t/Other.swiftmodule %S/Inputs/typed_throws_cmo_other.swift
// RUN: %target-swift-frontend -emit-ir -parse-as-library -O -enable-default-cmo -module-name test -I %t %s | %FileCheck %s
//
// REQUIRES: CPU=powerpc64le

import Other

public enum MyErr: Error {
  case internalError(any Error)
  case nilStore
  case cancelled
}

public struct Catalog {
  public init() {}
}

// The function must be emitted (IRGen no longer aborts), and the error slot is
// tested/zeroed through an integer view rather than as an aggregate.
// CHECK-LABEL: define {{.*}} @"$s4test5index{{.*}}"
// CHECK-NOT:   icmp ne %T4test5MyErrO
// CHECK-NOT:   ptrtoint %T4test5MyErrO
public func index(_ s: Server, _ b: Bool, _ n: Int) async throws(MyErr) -> Catalog {
  do {
    let r = try await s.convert(b, n)
    _ = r.data.count
    return Catalog()
  } catch {
    let e = error as? MyErr ?? .internalError(error)
    throw e
  }
}
