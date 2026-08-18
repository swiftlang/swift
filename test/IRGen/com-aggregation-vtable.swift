// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -I %t -emit-ir %s | %FileCheck %s

@com(interface: "43000000-0000-0000-0000-000000000001")
protocol IWidget {
}

@com
final class Widget: IWidget {
}

@com
final class AggregatedWidget: IWidget, COMAggregatable {
  let controller: (any IUnknown)? = nil
}

// A standalone implementation uses the ordinary identity and lifetime
// operations for each of its physical interface projections.

// CHECK-DAG: @"$s{{.*}}6WidgetCMn.com.vtable.$s3COM12ISwiftObjectMp" = private constant {{.*}} ptr @QueryInterface, ptr @AddRef, ptr @Release
// CHECK-DAG: @"$s{{.*}}6WidgetCMn.com.vtable.$s{{.*}}7IWidgetMp" = private constant {{.*}} ptr @QueryInterface, ptr @AddRef, ptr @Release

// An aggregatable implementation uses the delegating operation trio for every
// projection, including the compiler-managed Swift identity interface.

// CHECK-DAG: @"$s{{.*}}16AggregatedWidgetCMn.com.vtable.$s3COM12ISwiftObjectMp" = private constant {{.*}} ptr @AggregatedQueryInterface, ptr @AggregatedAddRef, ptr @AggregatedRelease
// CHECK-DAG: @"$s{{.*}}16AggregatedWidgetCMn.com.vtable.$s{{.*}}7IWidgetMp" = private constant {{.*}} ptr @AggregatedQueryInterface, ptr @AggregatedAddRef, ptr @AggregatedRelease
