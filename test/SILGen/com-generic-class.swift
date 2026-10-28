// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir -sil-verify-all %s | %FileCheck %s --check-prefix=IR

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IClassItem: AnyObject {}

// AnyObject does not establish Swift reference counting for a COM argument.
// CHECK-LABEL: sil {{.*}}@$s{{.*}}4copy{{.*}} : $@convention(thin) <T where T : IClassItem> (@in_guaranteed T) -> @out T
// CHECK: copy_addr
// IR-LABEL: define{{.*}} @"$s{{.*}}4copy
// IR-NOT: swift_unknownObjectRetain
// IR-NOT: swift_retain
// IR: InitializeWithCopy
// IR: ret void
public func copy<T: IClassItem>(_ value: borrowing T) -> T { copy value }

// Ordinary class-bound generics retain their direct reference representation.
// CHECK-LABEL: sil {{.*}}@$s{{.*}}6native{{.*}} : $@convention(thin) <T where T : AnyObject> (@guaranteed T) -> @owned T
// CHECK: strong_retain
public func native<T: AnyObject>(_ value: borrowing T) -> T { copy value }

// A concrete superclass still establishes the native object representation.
public class Base {}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}10superclass{{.*}} : $@convention(thin) <T where T : Base, T : IClassItem> (@guaranteed T) -> @owned T
// CHECK: strong_retain
public func superclass<T: Base>(_ value: borrowing T) -> T where T: IClassItem {
  copy value
}

// CHECK-LABEL: sil {{.*}}@$s{{.*}}7capture{{.*}} : $@convention(thin) <T where T : IClassItem> (@in_guaranteed T) -> @owned @callee_guaranteed {{.*}}() -> @out
// CHECK: copy_addr
// CHECK: partial_apply
public func capture<T: IClassItem>(_ value: T) -> () -> T { { value } }
