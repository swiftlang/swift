// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s -o - | %FileCheck %s

import COM

@com(interface: "45000000-0000-0000-0000-000000000001")
public protocol IItem {
}

@com(interface: "45000000-0000-0000-0000-000000000002")
public protocol IProvider {
  func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt
}

// CHECK-LABEL: sil {{.*}}getItem
// CHECK:         [[ITEM:%.*]] = alloc_stack {{.*}}$Optional<any IItem>
// CHECK:         [[NONE:%.*]] = enum $Optional<any IItem>, #Optional.none!enumelt
// CHECK:         store [[NONE]] to [[ITEM]]
// CHECK:         [[ACCESS:%.*]] = begin_access [modify] [static] [[ITEM]]
// CHECK:         [[POINTER:%.*]] = address_to_pointer {{.*}} [[ACCESS]]
// CHECK:         [[TYPED:%.*]] = struct $UnsafeMutablePointer<Optional<any IItem>> ([[POINTER]])
// CHECK:         [[ARGUMENT:%.*]] = enum $Optional<UnsafeMutablePointer<Optional<any IItem>>>, #Optional.some!enumelt, [[TYPED]]
// CHECK:         [[METHOD:%.*]] = com_method {{.*}}, #IProvider.GetItem
// CHECK:         apply [[METHOD]]<{{.*}}>({{ *}}[[ARGUMENT]],
// CHECK:         end_access [[ACCESS]]
func getItem(_ provider: borrowing any IProvider) -> (any IItem)? {
  var item: (any IItem)?
  _ = provider.GetItem(&item)
  return item
}

@com(implementation: "45000000-0000-0000-0000-000000000003")
open class CObject: IProvider {
  public init() {
  }

  open func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> CInt {
    0
  }
}

// CHECK-LABEL: sil [transparent] [thunk] {{.*}}CObjectC{{.*}}GetItem{{.*}}TWV
// CHECK-SAME:  $@convention(com_method) (Optional<UnsafeMutablePointer<Optional<any IItem>>>, @guaranteed CObject) -> Int32
