// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s -o - | %FileCheck %s

import COM

@com(interface: "45000000-0000-0000-0000-000000000001")
public protocol IItem {
}

@com(interface: "45000000-0000-0000-0000-000000000002")
public protocol IProvider {
  func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt
}

// The pointer retains its typed Swift spelling but crosses the foreign ABI as
// an opaque double pointer. Dispatch forwards that pointer without an adapter.

// CHECK-LABEL: define {{.*}}passItem
// CHECK-SAME:  (ptr [[PROVIDER:%.*]], ptr [[ITEM:%.*]])
// CHECK:         [[INTERFACE:%.*]] = load ptr, ptr [[PROVIDER_ADDRESS:%[^,]+]]
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:         [[SELF:%.*]] = load ptr, ptr [[PROVIDER_ADDRESS]]
// CHECK:         [[RESULT:%.*]] = call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], ptr [[ITEM]])
// CHECK:         ret i32 [[RESULT]]
public func passItem(_ provider: borrowing any IProvider,
                     _ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
  provider.GetItem(item)
}

@com(implementation: "45000000-0000-0000-0000-000000000003")
open class CObject: IProvider {
  public init() {
  }

  open func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
    0
  }
}

// The native entry has the same two-pointer ABI as a hand-written COM entry.
// It recovers the Swift object from the interface pointer and dynamically
// dispatches the open requirement through the class vtable.

// CHECK-LABEL: define {{.*}}i32 {{.*}}CObjectC{{.*}}GetItem{{.*}}TWV
// CHECK-SAME:  (ptr [[PTHIS:%.*]], ptr [[ITEM:%.*]])
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[PTHIS]]
// CHECK:         [[ADJUSTMENT_SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i32 -1
// CHECK:         [[ADJUSTMENT:%.*]] = load i{{32|64}}, ptr [[ADJUSTMENT_SLOT]]
// CHECK:         [[OBJECT:%.*]] = getelementptr inbounds i8, ptr [[PTHIS]], i{{32|64}} [[ADJUSTMENT]]
// CHECK:         [[METHOD:%.*]] = load ptr, ptr {{%.*}}, {{.*}}!invariant.load
// CHECK:         [[RESULT:%.*]] = call swiftcc i32 [[METHOD]](ptr [[ITEM]], ptr swiftself [[OBJECT]])
// CHECK:         ret i32 [[RESULT]]
