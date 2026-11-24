// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen -sil-verify-all -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s -o /dev/null

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValue {}

struct Holder {
  unowned(unsafe) var value: any IValue
  unowned(unsafe) var optional: (any IValue)?
}

// CHECK-LABEL: sil hidden [ossa] {{.*}}4read
// CHECK: load [trivial] {{.*}}$*@sil_unmanaged any IValue
// CHECK: strong_copy_unmanaged_value {{.*}}$@sil_unmanaged any IValue
// CHECK: return {{.*}}$any IValue
func read(_ holder: inout Holder) -> any IValue {
  holder.value
}

// CHECK-LABEL: sil hidden [ossa] {{.*}}12readOptional
// CHECK: load [trivial] {{.*}}$*@sil_unmanaged Optional<any IValue>
// CHECK: strong_copy_unmanaged_value {{.*}}$@sil_unmanaged Optional<any IValue>
// CHECK: return {{.*}}$Optional<any IValue>
func readOptional(_ holder: inout Holder) -> (any IValue)? {
  holder.optional
}

// CHECK-LABEL: sil hidden [ossa] {{.*}}5write
// CHECK: ref_to_unmanaged {{.*}}$any IValue to $@sil_unmanaged any IValue
// CHECK: ref_to_unmanaged {{.*}}$Optional<any IValue> to $@sil_unmanaged Optional<any IValue>
func write(_ holder: inout Holder, _ value: borrowing any IValue) {
  holder.value = copy value
  holder.optional = copy value
}
