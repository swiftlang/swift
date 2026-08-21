// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s | %FileCheck %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-sil -sil-verify-all %s | %FileCheck %s --check-prefix=DEFAULT

@com(interface: "20000000-0000-0000-0000-000000000001")
public protocol IWidget {
  func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32
}

extension IWidget {
  public func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 17
    return 0
  }
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

// CHECK-LABEL: sil private [transparent] [thunk] {{.*}}TWV
// CHECK-SAME:  $@convention(com_method) (Optional<UnsafeMutablePointer<Int32>>, @guaranteed Widget) -> UInt32
// CHECK:          bb0({{.*}}, [[SELF:%.*]] : $Widget):
// CHECK-NOT:      load
// CHECK:          function_ref {{.*}}WidgetC5value
// CHECK:          apply {{.*}}({{.*}}, [[SELF]])
// CHECK:          return

@com
public class DefaultWidget: IWidget {
}

// A non-final implementation inherits a protocol-extension witness through a
// class-bound archetype. Its native entry receives the recovered object
// directly; the COM ABI does not carry a separate Self metadata argument.

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] {{.*}}DefaultWidgetC{{.*}}TWV
// CHECK-SAME: $@convention(com_method) <[[DEFAULT_SELF:[^ ]+]] where [[DEFAULT_SELF]] : DefaultWidget>
// CHECK-SAME: (Optional<UnsafeMutablePointer<Int32>>, @guaranteed [[DEFAULT_SELF]]) -> UInt32

@com
public final class PublicWidget: IWidget {
}

// A serialized conformance retains the native entry alongside its ordinary
// witness. Both thunks therefore have shared linkage and serialized bodies.

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] {{.*}}PublicWidgetC{{.*}}TWV
// CHECK-SAME: $@convention(com_method) (Optional<UnsafeMutablePointer<Int32>>, @guaranteed PublicWidget) -> UInt32

@com
open class OpenWidget: IWidget {
  public init() {
  }

  open func value(_ result: UnsafeMutablePointer<Int32>?) -> UInt32 {
    result?.pointee = 17
    return 0
  }
}

// An external subclass uses this entry when it builds its inherited COM
// interface vtable. The witness-table reference causes the declaration to be
// serialized with the open conformance.

// CHECK-LABEL: sil [transparent] [thunk] {{.*}}OpenWidgetC{{.*}}TWV
// CHECK-SAME: $@convention(com_method) <[[OPEN_SELF:[^ ]+]] where [[OPEN_SELF]] : OpenWidget>
// CHECK-SAME: (Optional<UnsafeMutablePointer<Int32>>, @guaranteed [[OPEN_SELF]]) -> UInt32

// The witness tables record the native entries separately from the ordinary
// Swift witnesses. IRGen follows these references when it builds COM vtables.

// CHECK-LABEL: sil_witness_table hidden Widget: IWidget
// CHECK: method #IWidget.value: {{.*}} : {{.*}}TW{{.*}}, com {{.*}}TWV
// CHECK-LABEL: sil_witness_table DefaultWidget: IWidget
// CHECK: method #IWidget.value: {{.*}} : {{.*}}TW{{.*}}, com {{.*}}TWV
// CHECK-LABEL: sil_witness_table [serialized] PublicWidget: IWidget
// CHECK: method #IWidget.value: {{.*}} : {{.*}}TW{{.*}}, com {{.*}}TWV
// CHECK-LABEL: sil_witness_table OpenWidget: IWidget
// CHECK: method #IWidget.value: {{.*}} : {{.*}}TW{{.*}}, com {{.*}}TWV

// The synthesized ISwiftObject conformance uses the default property
// implementations supplied by the COM module.

// DEFAULT-DAG: sil private [transparent] [thunk] {{.*}}WidgetC{{.*}}ISwiftObject{{.*}}6object{{.*}}TWV
// DEFAULT-SAME: $@convention(com_method)
// DEFAULT-DAG: sil private [transparent] [thunk] {{.*}}WidgetC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TWV
// DEFAULT-SAME: $@convention(com_method)
// DEFAULT-DAG: sil shared [transparent] [serialized] [thunk] {{.*}}DefaultWidgetC{{.*}}ISwiftObject{{.*}}6object{{.*}}TWV : $@convention(com_method) <[[DEFAULT_OBJECT_SELF:[^ ]+]] where [[DEFAULT_OBJECT_SELF]] : DefaultWidget> (@guaranteed [[DEFAULT_OBJECT_SELF]]) -> UnsafeMutableRawPointer
// DEFAULT-DAG: sil shared [transparent] [serialized] [thunk] {{.*}}DefaultWidgetC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TWV : $@convention(com_method) <[[DEFAULT_METADATA_SELF:[^ ]+]] where [[DEFAULT_METADATA_SELF]] : DefaultWidget> (@guaranteed [[DEFAULT_METADATA_SELF]]) -> UnsafeRawPointer
// DEFAULT-DAG: sil [transparent] [thunk] {{.*}}OpenWidgetC{{.*}}ISwiftObject{{.*}}6object{{.*}}TWV : $@convention(com_method) <[[OPEN_OBJECT_SELF:[^ ]+]] where [[OPEN_OBJECT_SELF]] : OpenWidget> (@guaranteed [[OPEN_OBJECT_SELF]]) -> UnsafeMutableRawPointer
// DEFAULT-DAG: sil [transparent] [thunk] {{.*}}OpenWidgetC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TWV : $@convention(com_method) <[[OPEN_METADATA_SELF:[^ ]+]] where [[OPEN_METADATA_SELF]] : OpenWidget> (@guaranteed [[OPEN_METADATA_SELF]]) -> UnsafeRawPointer
// DEFAULT-LABEL: sil_witness_table {{.*}} Widget: ISwiftObject
// DEFAULT-DAG: method #ISwiftObject.object!getter: {{.*}}, com {{.*}}TWV
// DEFAULT-DAG: method #ISwiftObject.metadata!getter: {{.*}}, com {{.*}}TWV
