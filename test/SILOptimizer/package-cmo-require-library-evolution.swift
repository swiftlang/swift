// RUN: %empty-directory(%t)

// RUN: not %target-build-swift %s \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -emit-module -o %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -package-cmo \
// RUN: -Xfrontend -allow-non-resilient-access \
// RUN: -O -wmo 2>&1 | %FileCheck %s --check-prefix CHECK-DIAGS
// CHECK-DIAGS: error: Library evolution must be enabled for Package CMO

// RUN: %target-build-swift %s \
// RUN: -module-name=Lib -package-name Pkg \
// RUN: -emit-module -o %t/Lib.swiftmodule -I%t \
// RUN: -Xfrontend -package-cmo \
// RUN: -Xfrontend -allow-non-resilient-access \
// RUN: -enable-library-evolution \
// RUN: -O -wmo

// RUN: llvm-bcanalyzer %t/Lib.swiftmodule | %FileCheck %s -check-prefix=CHECK-BC
// CHECK-BC: SERIALIZE_PACKAGE_ENABLED

// REQUIRES: swift_in_compiler

public struct S {
  public init() {}
  package var x: Int {
    get { return 0 }
    set {}
  }
  package func f() -> Int {
    return 1
  }
}
