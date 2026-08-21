// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil %s | %FileCheck %s --check-prefix=DEFAULT

@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32
}

@com
final class Widget: IWidget {
  @inline(never)
  func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 42
    return result == nil ? 1 : 0
  }
}

// The ordinary Swift witness remains the conformance entry.

// CHECK-LABEL: sil private [transparent] [thunk] {{.*}}TW
// CHECK-SAME: $@convention(witness_method:

// Native COM vtables use a sibling thunk with the same requirement
// adaptation and the foreign COM method convention.

// CHECK-LABEL: sil private [transparent] [thunk] [used] {{.*}}TW.com.entry
// CHECK-SAME:  $@convention(com_method) (Optional<UnsafeMutablePointer<Int32>>, @guaranteed Widget) -> UInt32
// CHECK:          bb0({{.*}}, [[SELF:%.*]] : $Widget):
// CHECK-NOT:      load
// CHECK:          function_ref {{.*}}WidgetC5value
// CHECK:          apply {{.*}}({{.*}}, [[SELF]])
// CHECK:          return

// The synthesized ISwiftObject conformance uses the default property
// implementations supplied by the COM module.

// DEFAULT-DAG: sil private [transparent] [thunk] [used] {{.*}}WidgetC{{.*}}ISwiftObject{{.*}}6object{{.*}}TW.com.entry
// DEFAULT-SAME: $@convention(com_method)
// DEFAULT-DAG: sil private [transparent] [thunk] [used] {{.*}}WidgetC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TW.com.entry
// DEFAULT-SAME: $@convention(com_method)
// DEFAULT-LABEL: sil_witness_table {{.*}} Widget: ISwiftObject
// DEFAULT-DAG: method #ISwiftObject.object!getter:
// DEFAULT-DAG: method #ISwiftObject.metadata!getter:
