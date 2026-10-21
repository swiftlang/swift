// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -O -wmo -enable-default-cmo -parse-as-library %t/Lib.swift -emit-module-path=%t/Lib.swiftmodule -module-name=Lib
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %t/Client.swift -I%t | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -O -wmo -cross-module-optimization -parse-as-library %t/Lib.swift -emit-module-path=%t/Lib.swiftmodule -module-name=Lib
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %t/Client.swift -I%t | %FileCheck %s

// https://github.com/swiftlang/swift/issues/93420
// Within Lib, the opaque result types are looked through down to `S`. But a
// client cannot look through the opaque result type of the private `c()`.
// Therefore functions which refer to `b()` must not be serialized.

//--- Lib.swift

public protocol P {}
public struct S: P { public init() {} }

public func a() -> some P { b() }
public func b() -> some P { c() }
private func c() -> some P { S() }

public func d() -> Any { b() }

@inline(never)
public func e() -> some P { c() }
public func f() -> some P { e() }

extension P {
  public func ma() -> some P { mb() }
  public func mb() -> some P { mc() }
  private func mc() -> some P { S() }
}

public func pa() -> some P { pb() }
public func pb() -> some P { pc() }
public func pc() -> some P { S() }

//--- Client.swift

import Lib

// CHECK-LABEL: sil @$s6Client5testAyyF :
// CHECK:         function_ref @$s3Lib1aQryF
// CHECK:       } // end sil function '$s6Client5testAyyF'
public func testA() { _ = a() }

// CHECK-LABEL: sil @$s6Client5testDypyF :
// CHECK:         function_ref @$s3Lib1dypyF
// CHECK:       } // end sil function '$s6Client5testDypyF'
public func testD() -> Any { d() }

// CHECK-LABEL: sil @$s6Client5testFyyF :
// CHECK:         function_ref @$s3Lib1fQryF
// CHECK:       } // end sil function '$s6Client5testFyyF'
public func testF() { _ = f() }

// CHECK-LABEL: sil @$s6Client6testMAyyF :
// CHECK:         function_ref @$s3Lib1PPAAE2maQryF
// CHECK:       } // end sil function '$s6Client6testMAyyF'
public func testMA() { _ = S().ma() }

// The underlying types of a fully public chain are visible to the client.
// CHECK-LABEL: sil @$s6Client6testPA3Lib1P_pyF :
// CHECK-NOT:     function_ref
// CHECK:       } // end sil function '$s6Client6testPA3Lib1P_pyF'
public func testPA() -> any P { pa() }
