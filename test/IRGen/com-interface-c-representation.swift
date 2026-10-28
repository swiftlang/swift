// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -sil-verify-all -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D LIBRARY -module-name Interfaces -enable-library-evolution -emit-module-path %t/Interfaces.swiftmodule %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -D CLIENT -module-name M -sil-verify-all -emit-ir %s | %FileCheck %s

#if CLIENT
import Interfaces
#else
@com(interface: "46000000-0000-0000-0000-000000000001")
public protocol IItem {}

@com(interface: "46000000-0000-0000-0000-000000000002")
public protocol IProvider {
  func take(_ item: any IItem)
  func echo(_ item: (any IItem)?) -> (any IItem)?
  func GetItem(_ result: UnsafeMutablePointer<(any IItem)?>?) -> Int32
  func inspect(_ items: UnsafePointer<any IItem>)
}
#endif

#if !LIBRARY
// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M4take
// CHECK-SAME: (ptr {{%.*}}, ptr [[ITEM:%[^,) ]+]])
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 3
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[METHOD]](ptr {{%.*}}, ptr [[ITEM]])
public func take(_ provider: borrowing any IProvider, _ item: borrowing any IItem) {
  provider.take(item)
}

// CHECK-LABEL: define{{.*}} swiftcc {{.*}} @"$s1M4echo
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 4
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}ptr [[METHOD]](ptr {{%.*}}, ptr {{%.*}})
public func echo(_ provider: borrowing any IProvider, _ item: (any IItem)?) -> (any IItem)? {
  provider.echo(item)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s1M3get
// CHECK-SAME: (ptr {{%.*}}, ptr [[OUT:%[^,) ]+]])
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 5
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr {{%.*}}, ptr [[OUT]])
public func get(_ provider: borrowing any IProvider, _ result: UnsafeMutablePointer<(any IItem)?>?) -> Int32 {
  provider.GetItem(result)
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s1M7inspect
// CHECK-SAME: (ptr {{%.*}}, ptr [[ITEMS:%[^,) ]+]])
// CHECK: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr {{%.*}}, i{{32|64}} 6
// CHECK: [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK: call {{(x86_stdcallcc )?}}void [[METHOD]](ptr {{%.*}}, ptr [[ITEMS]])
public func inspect(_ provider: borrowing any IProvider, _ items: UnsafePointer<any IItem>) {
  provider.inspect(items)
}

// CHECK-LABEL: define{{.*}} void @accept_item(ptr {{%.*}})
@c(accept_item)
public func accept(_ item: any IItem) {}

// CHECK-LABEL: define{{.*}} void @fill_item(ptr {{%.*}})
@c(fill_item)
public func fill(_ item: UnsafeMutablePointer<(any IItem)?>?) {}
#endif
