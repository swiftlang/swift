// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir %s | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t -emit-ir %s | %FileCheck %s -check-prefix CHECK-CF

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase: IUnknown {
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IIndependent: IUnknown {
}

@com(interface: "10000000-0000-0000-0000-000000000004")
public protocol IRootless {
}

@com
public class Base: IBase {
  required public init() {
  }
}

public final class MultiRootImplementation: Base, IDerived, IIndependent {
  required public init() {
  }
}

@com
public final class RootlessImplementation: IRootless {
  public init() {
  }
}

// The interface map contains one protocol-descriptor reference for every
// logical interface exposed by the subclass.

// CHECK:      @"$s{{.*}}23MultiRootImplementationCMn.com.interface_map" = private constant {{.*}}<{ i32 5, i32 0,
// CHECK-SAME:   $s3COM12ISwiftObjectMp
// CHECK-SAME:   $s3COM8IUnknownMp
// CHECK-SAME:   $s{{.*}}5IBaseMp
// CHECK-SAME:   $s{{.*}}8IDerivedMp
// CHECK-SAME:   $s{{.*}}12IIndependentMp

// Microsoft COM adds IUnknown as the model root. CoreFoundation leaves the
// source hierarchy rootless. Both models add the compiler-managed Swift
// identity.

// CHECK:      @"$s{{.*}}22RootlessImplementationCMn.com.interface_map" = private constant {{.*}}<{ i32 3, i32 0,
// CHECK-SAME:   $s3COM12ISwiftObjectMp
// CHECK-SAME:   $s4main9IRootlessMp
// CHECK-SAME:   $s3COM8IUnknownMp

// CHECK-CF:      @"$s{{.*}}22RootlessImplementationCMn.com.interface_map" = private constant {{.*}}<{ i32 2, i32 0,
// CHECK-CF-SAME:   $s3COM12ISwiftObjectMp
// CHECK-CF-SAME:   $s4main9IRootlessMp
