// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen -sil-verify-all %s | %FileCheck %s

@com(interface: "46000000-0000-0000-0000-000000000001")
protocol IWidget {
}

@inline(never)
func modify<T: IWidget>(_ value: inout T) {
}

// Opening an inout COM existential preserves its one-word address.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}15openExistential
// CHECK:       [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr mutable_access [[ACCESS]] to $*@opened({{.*}}, any IWidget) Self
// CHECK:       apply {{%.*}}<@opened({{.*}}, any IWidget) Self>([[OPENED]])
// CHECK:       end_access [[ACCESS]]
func openExistential(_ value: inout any IWidget) {
  modify(&value)
}

struct Storage {
  var value: any IWidget
}

// Addressable member chains use the same opening operation.

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}10openMember
// CHECK:       [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// CHECK:       [[MEMBER:%.*]] = struct_element_addr [[ACCESS]], #Storage.value
// CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr mutable_access [[MEMBER]] to $*@opened({{.*}}, any IWidget) Self
// CHECK:       apply {{%.*}}<@opened({{.*}}, any IWidget) Self>([[OPENED]])
// CHECK:       end_access [[ACCESS]]
func openMember(_ storage: inout Storage) {
  modify(&storage.value)
}
