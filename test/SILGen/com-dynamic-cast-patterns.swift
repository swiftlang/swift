// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -emit-silgen -sil-verify-all %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -emit-sil -sil-verify-all %s -o /dev/null
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name M -O -emit-sil -sil-verify-all %s -o /dev/null

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol ISource { func value() -> Int32 }

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol ITarget { func value() -> Int32 }

// CHECK-LABEL: sil [ossa] @$s1M7pattern
// CHECK: [[COPYABLE:%[0-9]+]] = moveonlywrapper_to_copyable [guaranteed]
// CHECK: [[SOURCE:%[0-9]+]] = alloc_stack $any ISource
// CHECK: [[BORROW:%[0-9]+]] = store_borrow [[COPYABLE]] to [[SOURCE]]
// CHECK: [[DEST:%[0-9]+]] = alloc_stack $any ITarget
// CHECK: checked_cast_addr_br copy_on_success any ISource in [[BORROW]] to any ITarget in [[DEST]], [[SUCCESS:bb[0-9]+]], [[FAILURE:bb[0-9]+]]
// CHECK: [[SUCCESS]]:
// CHECK: [[RESULT:%[0-9]+]] = load [take] [[DEST]]
// CHECK: destroy_value [[RESULT]]
// CHECK: dealloc_stack [[DEST]]
// CHECK: end_borrow [[BORROW]]
// CHECK: dealloc_stack [[SOURCE]]
// CHECK: [[FAILURE]]:
// CHECK: dealloc_stack [[DEST]]
// CHECK: end_borrow [[BORROW]]
// CHECK: dealloc_stack [[SOURCE]]
public func pattern(_ source: borrowing any ISource) -> Bool {
  switch source {
  case is any ITarget: return true
  default: return false
  }
}

// CHECK-LABEL: sil [ossa] @$s1M7binding
// CHECK: checked_cast_addr_br copy_on_success any ISource in
// CHECK-SAME: to any ITarget in
public func binding(_ source: borrowing any ISource) -> (any ITarget)? {
  switch source {
  case let target as any ITarget: return target
  default: return nil
  }
}

// CHECK-LABEL: sil [ossa] @$s1M7guarded
// CHECK: checked_cast_addr_br copy_on_success any ISource in {{.*}} to any ITarget in [[DEST:%[0-9]+]], [[MATCH:bb[0-9]+]], [[FAILURE:bb[0-9]+]]
// CHECK: [[MATCH]]:
// CHECK: [[RESULT:%[0-9]+]] = load [take] [[DEST]]
// CHECK: [[TARGET:%[0-9]+]] = move_value [lexical] [var_decl] [[RESULT]]
// CHECK: com_method {{.*}}, #ITarget.value
// CHECK: cond_br {{.*}}, [[BODY:bb[0-9]+]], [[GUARDFAIL:bb[0-9]+]]
// CHECK: [[BODY]]:
// CHECK: com_method {{.*}}, #ITarget.value
// CHECK: destroy_value [[TARGET]]
// CHECK: [[GUARDFAIL]]:
// CHECK: destroy_value [[TARGET]]
// CHECK: br [[DEFAULT:bb[0-9]+]]
// CHECK: [[FAILURE]]:
// CHECK: br [[DEFAULT]]
// CHECK: [[DEFAULT]]:
// CHECK: com_method {{.*}}, #ISource.value
public func guarded(_ source: borrowing any ISource) -> Int32 {
  switch source {
  case let target as any ITarget where target.value() > 0:
    return target.value()
  default:
    return source.value()
  }
}

// CHECK-LABEL: sil [ossa] @$s1M13erasedPattern
// CHECK: checked_cast_addr_br copy_on_success Any in
public func erasedPattern(_ source: borrowing Any) -> Bool {
  switch copy source {
  case is any ITarget: return true
  default: return false
  }
}

// CHECK-LABEL: sil [ossa] @$s1M14genericPattern
// CHECK: checked_cast_addr_br copy_on_success T in
public func genericPattern<T>(_ source: borrowing T) -> Bool {
  switch copy source {
  case is any ITarget: return true
  default: return false
  }
}

// CHECK-LABEL: sil [ossa] @$s1M16consumingPattern
// CHECK: checked_cast_addr_br copy_on_success any ISource in
public func consumingPattern(_ source: consuming any ISource) -> (any ITarget)? {
  switch source {
  case let target as any ITarget: return target
  default: return nil
  }
}

// CHECK-LABEL: sil [ossa] @$s1M11typePattern
// CHECK: checked_cast_br
public func typePattern(_ source: Any.Type) -> Bool {
  switch source {
  case is any ITarget.Type: return true
  default: return false
  }
}
