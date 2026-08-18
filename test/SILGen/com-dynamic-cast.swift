// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource { }

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget { }

// CHECK-LABEL: sil [ossa] @$s{{.*}}11conditionalyAA7ITarget_pSgAA7ISource_pF
// CHECK-SAME:  @guaranteed any ISource
// CHECK:         [[COPYABLE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK:         [[SOURCE:%.*]] = alloc_stack $any ISource
// CHECK:         [[BORROW:%.*]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK:         checked_cast_addr_br copy_on_success any ISource in [[BORROW]]
// CHECK-SAME:    to any ITarget in
public func conditional(_ source: borrowing any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}6forcedyAA7ITarget_pAA7ISource_pF
// CHECK-SAME:  @guaranteed any ISource
// CHECK:         [[COPYABLE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK:         [[SOURCE:%.*]] = alloc_stack $any ISource
// CHECK:         [[BORROW:%.*]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK:         checked_cast_addr_br copy_on_success any ISource in [[BORROW]]
// CHECK-SAME:    to any ITarget in
public func forced(_ source: borrowing any ISource) -> any ITarget {
  source as! any ITarget
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4testySbAA7ISource_pF
// CHECK-SAME:  @guaranteed any ISource
// CHECK:         [[COPYABLE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK:         [[BORROW:%.*]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK:         checked_cast_addr_br copy_on_success any ISource in [[BORROW]]
// CHECK-SAME:    to any ITarget in
public func test(_ source: borrowing any ISource) -> Bool {
  source is any ITarget
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7patternySbAA7ISource_pF
// CHECK-SAME:  @guaranteed any ISource
// CHECK:         [[COPYABLE:%.*]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK:         [[SOURCE:%.*]] = alloc_stack $any ISource
// CHECK:         [[BORROW:%.*]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK:         checked_cast_addr_br copy_on_success any ISource in [[BORROW]]
// CHECK-SAME:    to any ITarget in
public func pattern(_ source: borrowing any ISource) -> Bool {
  switch source {
  case is any ITarget:
    return true
  default:
    return false
  }
}
