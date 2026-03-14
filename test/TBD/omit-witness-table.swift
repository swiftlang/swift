// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -module-name test -validate-tbd-against-ir=all %s -enable-library-evolution -emit-tbd -emit-tbd-path %t.result.tbd -tbd-is-installapi -parse-as-library -emit-ir -o/dev/null -tbd-install_name test
// RUN: %llvm-nm -g %t.result.tbd | %FileCheck %s 

public enum TestError : Error {
  case unsupportedVersion(Int)
}

// CHECK:      T _$s4test9TestErrorO18unsupportedVersionyACSicACmFWC
// CHECK-NEXT: T _$s4test9TestErrorOMa
// CHECK-NEXT: T _$s4test9TestErrorOMn
// CHECK-NEXT: D _$s4test9TestErrorON
// CHECK-NEXT: T _$s4test9TestErrorOs0C0AAMc
