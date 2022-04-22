// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name main %s -emit-ir | %FileCheck %s --check-prefix=CHECK-REFL
// RUN: %target-swift-frontend -module-name main %s -emit-ir -reflection-metadata-for-debugger-only | %FileCheck %s --check-prefix=CHECK-REFLDEBUG
// RUN: %target-swift-frontend -module-name main %s -emit-ir -disable-reflection-metadata | %FileCheck %s --check-prefix=CHECK-NOREFL

public class Generic<T> {
  public func m1(t: T) -> T { return t }
  public func m2(t: T) -> T { return t }
}

protocol MyProtocol {
  associatedtype T
  func foo() -> T
}

public struct MyStruct<T>: MyProtocol {
  func foo() -> T { fatalError() }
}

// CHECK-REFL: @"$s4main8MyStructVyxGAA0B8ProtocolAAMA"
// CHECK-REFL: @"$s4main7GenericCMn" = {{.*}} @"$s4main7GenericCMF" {{.*}}
// CHECK-REFL: @"$s4main7GenericCMF" =
// CHECK-REFL: @"$s4main10MyProtocol_pMF" =
// CHECK-REFL: @"$s4main8MyStructVMn" = {{.*}} @"$s4main8MyStructVMF" {{.*}}
// CHECK-REFL: @"$s4main8MyStructVMF" =

// CHECK-REFLDEBUG-NOT: @"$s4main8MyStructVyxGAA0B8ProtocolAAMA"
// CHECK-REFLDEBUG: @"$s4main7GenericCMn" =
// CHECK-REFLDEBUG-NOT: @"$s4main7GenericCMF"
// CHECK-REFLDEBUG-SAME: , align 4
// CHECK-REFLDEBUG: @"$s4main7GenericCMF" =
// CHECK-REFLDEBUG: @"$s4main10MyProtocol_pMF" =
// CHECK-REFLDEBUG: @"$s4main8MyStructVMn" =
// CHECK-REFLDEBUG-NOT: @"$s4main8MyStructVMF"
// CHECK-REFLDEBUG-SAME: , align 4
// CHECK-REFLDEBUG: @"$s4main8MyStructVMF" =

// CHECK-NOREFL-NOT: @"$s4main8MyStructVyxGAA0B8ProtocolAAMA"
// CHECK-NOREFL-NOT: @"$s4main10MyProtocol_pMF"
// CHECK-NOREFL-NOT: @"$s4main7GenericCMF"
// CHECK-NOREFL-NOT: @"$s4main8MyStructVMF"
