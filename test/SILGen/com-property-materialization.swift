// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -sil-verify-all -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -sil-verify-all -emit-sil %s -o /dev/null

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IProperties: AnyObject {
  var value: CInt { get set }
  subscript(_ index: CInt) -> CInt { get set }
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}9increment
// CHECK: open_com_existential
// CHECK: [[GET:%.*]] = com_method {{%.*}}, #IProperties.value!getter
// CHECK: [[VALUE:%.*]] = apply [[GET]]
// CHECK: store [[VALUE]] to [trivial] [[TEMP:%[0-9]+]]
// CHECK-NOT: #IProperties.value!modify
// CHECK: [[SET:%.*]] = com_method {{%.*}}, #IProperties.value!setter
// CHECK: apply [[SET]]
// CHECK: dealloc_stack [[TEMP]]
// CHECK: return
public func increment(_ interface: any IProperties) {
  interface.value += 1
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4edit
// CHECK: open_com_existential
// CHECK: [[GET:%.*]] = com_method {{%.*}}, #IProperties.subscript!getter
// CHECK: apply [[GET]]
// CHECK-NOT: #IProperties.subscript!modify
// CHECK: try_apply {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[NORMAL]]
// CHECK: [[SET:%.*]] = com_method {{%.*}}, #IProperties.subscript!setter
// CHECK: apply [[SET]]
// CHECK: return
// CHECK: [[ERROR]]
// CHECK: [[SET:%.*]] = com_method {{%.*}}, #IProperties.subscript!setter
// CHECK: apply [[SET]]
// CHECK: throw
public func edit(_ interface: any IProperties, _ index: CInt,
                 _ body: (inout CInt) throws -> ()) rethrows {
  try body(&interface[index])
}

// Generic receivers continue to use Swift's coroutine witness.
// CHECK-LABEL: sil [ossa] @$s{{.*}}7generic
// CHECK-NOT: = com_method
// CHECK: [[MODIFY:%.*]] = witness_method $T, #IProperties.value!modify
// CHECK-SAME: $@yield_once @convention(witness_method: IProperties)
// CHECK: begin_apply [[MODIFY]]<T>
// CHECK: end_apply
// CHECK-NOT: = com_method
// CHECK: return
public func generic<T: IProperties>(_ interface: T) {
  interface.value += 1
}
