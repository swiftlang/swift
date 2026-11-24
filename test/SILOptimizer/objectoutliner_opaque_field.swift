// RUN: %target-swift-frontend -parse-as-library -O -module-name=test -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -O -module-name=test -emit-ir %s | %FileCheck %s -check-prefix=CHECK-IR

// A class with a stored property of opaque type has no fixed layout in IRGen,
// so its objects must not be statically initialized.

public protocol P {}
public struct S: P { public init() {} }

final class K {
  let n = 1
  let v: some P = S()
}

enum C {
  static let k = K()
}

public func getK() -> AnyObject { C.k }

// CHECK-NOT: sil_global private @$s4test1CO1k_WZTv_
// CHECK-IR-NOT: @"$s4test1CO1k_WZTv_" =
