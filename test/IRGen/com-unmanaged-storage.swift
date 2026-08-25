// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s -o - | %FileCheck %s

import COM

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
// CHECK:      call i32 [[ADDREF]](ptr [[INTERFACE]])
// CHECK:      ret ptr [[INTERFACE]]

// CHECK-LABEL: define{{.*}} swiftcc ptr @{{.*}}HolderCfd
// CHECK-NOT:  call i32
// CHECK:      ret ptr
