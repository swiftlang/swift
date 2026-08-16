// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -sil-verify-all -emit-silgen %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func method(_ value: CInt) -> CInt
}

extension IBase {
  var value: CInt {
    method(1)
  }
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived(_ value: CInt) -> CInt
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4base
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-NOT:     witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
public func base(_ interface: borrowing any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7refined
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IDerived
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IBase.method
// CHECK-SAME:    $@convention(com_method)
// CHECK-NOT:     witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
public func refined(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7derived
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IDerived
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[METHOD:%.*]] = com_method [[BORROW]], #IDerived.derived
// CHECK-SAME:    $@convention(com_method)
// CHECK-NOT:     witness_method
// CHECK:         apply [[METHOD]]<{{.*}}>({{%.*}}, [[BORROW]])
public func derived(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.derived(value)
}

// A Swift-only extension member opens the existential and calls the generic
// extension accessor. It does not use a COM vtable slot of its own.

// CHECK-LABEL: sil [ossa] @$s{{.*}}5value
// CHECK:         [[OPEN:%.*]] = open_com_existential {{%.*}} to $@opened{{.*}}IBase
// CHECK:         [[STORAGE:%.*]] = alloc_stack $@opened
// CHECK:         [[BORROW:%.*]] = store_borrow [[OPEN]] to [[STORAGE]]
// CHECK:         [[GETTER:%.*]] = function_ref @$s{{.*}}5IBasePAAE5values5Int32Vvg
// CHECK:         apply [[GETTER]]<@opened{{.*}}>([[BORROW]])
public func value(_ interface: borrowing any IBase) -> CInt {
  interface.value
}
