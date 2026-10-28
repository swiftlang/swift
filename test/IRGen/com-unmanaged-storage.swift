// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -disable-llvm-optzns -emit-ir %s -o - | %FileCheck %s

@com(interface: "AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE")
protocol IWidget {
}

final class Holder {
  unowned(unsafe) var interface: any IWidget
  unowned(unsafe) var optional: (any IWidget)?

  init(_ interface: any IWidget) {
    self.interface = interface
    self.optional = interface
  }
}

// Loading an unmanaged COM existential produces a strong value by calling
// AddRef through the stored interface pointer. Storing and destroying the
// unmanaged property itself perform no ownership operations.

// CHECK-LABEL: define{{.*}} swiftcc ptr @{{.*}}HolderC9interfaceAA7IWidget_pvg
// CHECK:      [[INTERFACE:%.*]] = load ptr
// CHECK:      icmp ne ptr [[INTERFACE]], null
// CHECK:      [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK:      [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 1
// CHECK:      [[ADDREF:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:      call {{(x86_stdcallcc )?}}i32 [[ADDREF]](ptr [[INTERFACE]])
// CHECK:      ret ptr [[INTERFACE]]

// CHECK-LABEL: define{{.*}} swiftcc ptr @{{.*}}HolderC8optionalAA7IWidget_pSgvg
// CHECK:      [[OPTIONAL:%.*]] = load ptr
// CHECK:      icmp ne ptr [[OPTIONAL]], null
// CHECK:      [[VTABLE:%.*]] = load ptr, ptr [[OPTIONAL]]
// CHECK:      [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 1
// CHECK:      [[ADDREF:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:      call {{(x86_stdcallcc )?}}i32 [[ADDREF]](ptr [[OPTIONAL]])
// CHECK:      ret ptr [[OPTIONAL]]

// CHECK-LABEL: define{{.*}} swiftcc ptr @{{.*}}HolderCfd
// CHECK-NOT:  call {{(x86_stdcallcc )?}}i32
// CHECK-NOT:  @swift_release
// CHECK:      ret ptr
