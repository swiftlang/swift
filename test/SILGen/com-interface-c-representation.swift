// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen -sil-verify-all %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s -o /dev/null

@com(interface: "47000000-0000-0000-0000-000000000001")
protocol IItem {}

@com(interface: "47000000-0000-0000-0000-000000000002")
protocol IProvider {
  func GetItem(_ result: UnsafeMutablePointer<(any IItem)?>?) -> Int32
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}7getItem
// CHECK: [[BOX:%[0-9]+]] = alloc_box ${ var Optional<any IItem> }
// CHECK: [[BORROW:%[0-9]+]] = begin_borrow {{.*}}[[BOX]]
// CHECK: [[ITEM:%[0-9]+]] = project_box [[BORROW]]
// CHECK: [[NONE:%[0-9]+]] = enum $Optional<any IItem>, #Optional.none!enumelt
// CHECK: store [[NONE]] to {{.*}}[[ITEM]]
// CHECK: [[ACCESS:%[0-9]+]] = begin_access [modify] [unknown] [[ITEM]]
// CHECK: [[POINTER:%[0-9]+]] = address_to_pointer {{.*}}[[ACCESS]]
// CHECK: [[CONVERT:%[0-9]+]] = function_ref @$ss30_convertInOutToPointerArgument
// CHECK: [[STORAGE:%[0-9]+]] = alloc_stack $UnsafeMutablePointer<Optional<any IItem>>
// CHECK: apply [[CONVERT]]<UnsafeMutablePointer<Optional<any IItem>>>([[STORAGE]], [[POINTER]])
// CHECK: [[TYPED:%[0-9]+]] = load [trivial] [[STORAGE]]
// CHECK: [[ARG:%[0-9]+]] = enum $Optional<UnsafeMutablePointer<Optional<any IItem>>>, #Optional.some!enumelt, [[TYPED]]
// CHECK: [[METHOD:%[0-9]+]] = com_method {{.*}}, #IProvider.GetItem
// CHECK: apply [[METHOD]]<{{.*}}>([[ARG]],
// CHECK: fix_lifetime [[ACCESS]]
// CHECK: end_access [[ACCESS]]
// CHECK: [[READ:%[0-9]+]] = begin_access [read] [unknown] [[ITEM]]
// CHECK: [[RESULT:%[0-9]+]] = load [copy] [[READ]]
// CHECK: return [[RESULT]]
func getItem(_ provider: borrowing any IProvider) -> (any IItem)? {
  var item: (any IItem)?
  _ = provider.GetItem(&item)
  return item
}
