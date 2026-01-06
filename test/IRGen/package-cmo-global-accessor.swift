 // RUN: %empty-directory(%t)

// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s -o %t/IR-nonres.ir
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s -enable-library-evolution -o %t/IR-res.ir
// RUN: %target-build-swift -module-name=File -package-name Pkg -I%t -emit-ir %s -enable-library-evolution -wmo -Xfrontend -experimental-allow-non-resilient-access -Xfrontend -experimental-package-cmo -o %t/IR-res-package-cmo.ir

// RUN: %FileCheck %s --check-prefix=CHECK-IR-NONRES < %t/IR-nonres.ir
// RUN: %FileCheck %s --check-prefix=CHECK-IR-RES < %t/IR-res.ir
// RUN: %FileCheck %s --check-prefix=CHECK-IR-RES < %t/IR-res-package-cmo.ir

public struct S {
  public static let x = "hello world"
  package static let y = "dogs cats"
}

public struct R {
  package static let a = "foo"
  public static let b = "bar"
}

// CHECK-IR-NONRES: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s4File1SV1xSSvau"()
// CHECK-IR-NONRES: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s4File1SV1ySSvau"()
// CHECK-IR-NONRES: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s4File1RV1aSSvau"()
// CHECK-IR-NONRES: define{{( dllexport)?}}{{( protected)?}} swiftcc ptr @"$s4File1RV1bSSvau"()

// CHECK-IR-RES: define hidden swiftcc ptr @"$s4File1SV1xSSvau"()
// CHECK-IR-RES: define hidden swiftcc ptr @"$s4File1SV1ySSvau"()
// CHECK-IR-RES: define hidden swiftcc ptr @"$s4File1RV1aSSvau"()
// CHECK-IR-RES: define hidden swiftcc ptr @"$s4File1RV1bSSvau"()

