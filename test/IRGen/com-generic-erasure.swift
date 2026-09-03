// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -disable-llvm-optzns -I %t -emit-ir %s | %FileCheck %s

@com(interface: "31000000-0000-0000-0000-000000000001")
public protocol IBase {
}

@com(interface: "31000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com(interface: "31000000-0000-0000-0000-000000000003")
public protocol IIndependent {
}

@com
public final class CObject: IDerived, IIndependent {
  public func derived(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 2
    return 0
  }
}

// A generic COM requirement passes the interface adjustment after metadata.
// Erasure applies that adjustment to the stored object.
// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6openedyAA8IDerived_pxAaCRzlF"
// CHECK-SAME:  (ptr noalias [[DERIVED_VALUE:%.*]], ptr %T, i{{32|64}} [[DERIVED_ADJUSTMENT:%T.IDerived]])
// CHECK:         [[DERIVED_OBJECT:%com.value[0-9]*]] = load ptr, ptr {{%.*}}
// CHECK:         [[DERIVED_INTERFACE:%com.interface[0-9]*]] = getelementptr inbounds i8, ptr [[DERIVED_OBJECT]], i{{32|64}} [[DERIVED_ADJUSTMENT]]
// CHECK:         ret ptr [[DERIVED_INTERFACE]]
public func opened<T: IDerived>(_ value: T) -> any IDerived {
  value
}

public func erased<T: IIndependent>(_ value: T) -> any IIndependent {
  value
}

// A distinct interface uses its own prefix adjustment.

public func independent(_ value: CObject) -> any IIndependent {
  erased(value)
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}11independent
// CHECK:         call swiftcc ptr @"$s{{.*}}6erased{{.*}}"(ptr {{.*}}, ptr {{.*}}, i{{32|64}} -{{(12|24)}})

// The primary interface uses the first prefix slot.

public func concrete(_ value: CObject) -> any IDerived {
  opened(value)
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}8concreteyAA8IDerived_pAA7CObjectCF
// CHECK:         call swiftcc ptr @"$s{{.*}}6opened{{.*}}"(ptr {{.*}}, ptr {{.*}}, i{{32|64}} -{{(8|16)}})

// Generic method dispatch applies the same adjustment for lookup and self.

public func generic<T: IDerived>(_ value: T,
                                 _ result: UnsafeMutablePointer<Int32>?)
    -> Int32 {
  value.derived(result)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}7genericys5Int32Vx_SpyADGSgtAA8IDerivedRzlF
// CHECK-SAME:  ptr %T, i{{32|64}} [[METHOD_ADJUSTMENT:%T.IDerived]]
// CHECK:         [[METHOD_OBJECT:%com.value[0-9]*]] = load ptr, ptr {{%.*}}
// CHECK:         [[METHOD_INTERFACE:%com.interface[0-9]*]] = getelementptr inbounds i8, ptr [[METHOD_OBJECT]], i{{32|64}} [[METHOD_ADJUSTMENT]]
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[METHOD_INTERFACE]]
// CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:         [[SELF_OBJECT:%com.value[0-9]*]] = load ptr, ptr {{%.*}}
// CHECK:         [[SELF_INTERFACE:%com.interface[0-9]*]] = getelementptr inbounds i8, ptr [[SELF_OBJECT]], i{{32|64}} [[METHOD_ADJUSTMENT]]
// CHECK:         call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF_INTERFACE]], ptr

// An opened existential is already at the requested interface address.
// Forwarding it through the generic ABI therefore passes adjustment zero.

public func forward(_ value: any IDerived) -> any IDerived {
  opened(value)
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}7forwardyAA8IDerived_pAaC_pF
// CHECK:         call swiftcc ptr @"$s{{.*}}6opened{{.*}}"(ptr {{.*}}, ptr {{.*}}, i{{32|64}} 0)
