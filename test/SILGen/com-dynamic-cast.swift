// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -emit-silgen -sil-verify-all %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -emit-sil -sil-verify-all %s -o /dev/null
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -O -emit-sil -sil-verify-all %s -o /dev/null

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource { func value() -> Int32 }

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget { func value() -> Int32 }

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IClassSource: AnyObject {}

// CHECK-LABEL: sil [ossa] @$s1M11conditional
// CHECK: [[COPYABLE:%[0-9]+]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK: [[SOURCE:%[0-9]+]] = alloc_stack $any ISource
// CHECK: [[BORROW:%[0-9]+]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK: [[DEST:%[0-9]+]] = alloc_stack $any ITarget
// CHECK: checked_cast_addr_br copy_on_success any ISource in [[BORROW]] to any ITarget in [[DEST]], [[SUCCESS:bb[0-9]+]], [[FAILURE:bb[0-9]+]]
// CHECK: [[SUCCESS]]:
// CHECK: [[RESULT:%[0-9]+]] = load [take] [[DEST]]
// CHECK: enum $Optional<any ITarget>, #Optional.some!enumelt, [[RESULT]]
// CHECK: dealloc_stack [[DEST]]
// CHECK: br [[CONT:bb[0-9]+]]
// CHECK: [[FAILURE]]:
// CHECK: enum $Optional<any ITarget>, #Optional.none!enumelt
// CHECK: dealloc_stack [[DEST]]
// CHECK: br [[CONT]]
// CHECK: [[CONT]](
// CHECK: end_borrow [[BORROW]]
// CHECK: dealloc_stack [[SOURCE]]
public func conditional(_ source: borrowing any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M6forced
// CHECK: [[COPYABLE:%[0-9]+]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK: [[SOURCE:%[0-9]+]] = alloc_stack $any ISource
// CHECK: [[BORROW:%[0-9]+]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK: [[DEST:%[0-9]+]] = alloc_stack $any ITarget
// CHECK-NEXT: unconditional_checked_cast_addr [copy] any ISource in [[BORROW]] to any ITarget in [[DEST]]
// CHECK-NEXT: [[RESULT:%[0-9]+]] = load [take] [[DEST]]
// CHECK-NEXT: dealloc_stack [[DEST]]
// CHECK-NEXT: end_borrow [[BORROW]]
// CHECK-NEXT: dealloc_stack [[SOURCE]]
// CHECK: return [[RESULT]]
public func forced(_ source: borrowing any ISource) -> any ITarget {
  source as! any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M4test
// CHECK: checked_cast_addr_br copy_on_success any ISource in
// CHECK-SAME: to any ITarget in
public func test(_ source: borrowing any ISource) -> Bool {
  source is any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M7consume
// CHECK: checked_cast_addr_br copy_on_success any ISource in
public func consume(_ source: consuming any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M7address
// CHECK: checked_cast_addr_br copy_on_success any ISource in
public func address(_ source: inout any ISource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M6erased
// CHECK: checked_cast_addr_br copy_on_success Any in
// CHECK-SAME: to any ITarget in
public func erased(_ source: borrowing Any) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M7generic
// CHECK: checked_cast_addr_br copy_on_success T in
// CHECK-SAME: to any ITarget in
public func generic<T>(_ source: borrowing T) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M13genericForced
// CHECK-NOT: explicit_copy_addr
// CHECK-NOT: checked_cast_addr_br
// CHECK: unconditional_checked_cast_addr [copy] T in {{%[0-9]+}} to any ITarget in [[DEST:%[0-9]+]]
// CHECK-NEXT: [[RESULT:%[0-9]+]] = load [take] [[DEST]]
// CHECK-NOT: unconditional_checked_cast_addr
// CHECK-NOT: explicit_copy_addr
// CHECK-NOT: unreachable
// CHECK: return [[RESULT]]
public func genericForced<T>(_ source: borrowing T) -> any ITarget {
  source as! any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M14optionalSource
// CHECK: checked_cast_addr_br copy_on_success any ISource in
public func optionalSource(_ source: borrowing (any ISource)?) -> (any ITarget)? {
  (copy source) as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M14optionalTarget
// CHECK: checked_cast_addr_br copy_on_success any ISource in
// CHECK-SAME: to Optional<any ITarget> in
public func optionalTarget(_ source: borrowing any ISource) -> (any ITarget)?? {
  source as? (any ITarget)?
}

// CHECK-LABEL: sil [ossa] @$s1M14optionalForced
// CHECK: unconditional_checked_cast_addr [copy] any ISource in
// CHECK-SAME: to Optional<any ITarget> in
public func optionalForced(_ source: borrowing any ISource) -> (any ITarget)? {
  source as! (any ITarget)?
}

// CHECK-LABEL: sil [ossa] @$s1M10classBound
// CHECK: checked_cast_addr_br copy_on_success any IClassSource in
public func classBound(_ source: borrowing any IClassSource) -> (any ITarget)? {
  source as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M8produced
// CHECK: [[VALUE:%[0-9]+]] = apply {{.*}}() : {{.*}} -> @owned any ISource
// CHECK: [[BORROW:%[0-9]+]] = begin_borrow [[VALUE]]
// CHECK: [[STORED:%[0-9]+]] = store_borrow [[BORROW]]
// CHECK: checked_cast_addr_br copy_on_success any ISource in [[STORED]]
// CHECK: end_borrow [[STORED]]
// CHECK: end_borrow [[BORROW]]
// CHECK: destroy_value [[VALUE]]
public func produced(_ source: () -> any ISource) -> (any ITarget)? {
  source() as? any ITarget
}

// CHECK-LABEL: sil [ossa] @$s1M14forcedProduced
// CHECK: [[VALUE:%[0-9]+]] = apply {{.*}}() : {{.*}} -> @owned any ISource
// CHECK: [[BORROW:%[0-9]+]] = begin_borrow [[VALUE]]
// CHECK: [[STORED:%[0-9]+]] = store_borrow [[BORROW]]
// CHECK: unconditional_checked_cast_addr [copy] any ISource in [[STORED]]
// CHECK-NOT: checked_cast_addr_br
// CHECK-NOT: explicit_copy_addr
// CHECK: end_borrow [[STORED]]
// CHECK: end_borrow [[BORROW]]
// CHECK: destroy_value [[VALUE]]
// CHECK: return
public func forcedProduced(_ source: () -> any ISource) -> any ITarget {
  source() as! any ITarget
}

// Metatype casts do not query an object for an interface.
// CHECK-LABEL: sil [ossa] @$s1M8metatype
// CHECK: checked_cast_br
public func metatype(_ source: Any.Type) -> (any ITarget.Type)? {
  source as? any ITarget.Type
}

public protocol Native {}

// CHECK-LABEL: sil [ossa] @$s1M6native
// CHECK: checked_cast_addr_br take_always Any in
// CHECK-SAME: to any Native in
public func native(_ source: Any) -> (any Native)? {
  source as? any Native
}
