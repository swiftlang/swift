// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Sema/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t -emit-ir %s | %FileCheck %s --implicit-check-not=@llvm.used

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget { }

@com
final class Widget: IWidget { }

// A canonical runtime implementation remains external to the client. Native
// vtables refer to its direct C entry points without placing the declarations
// in llvm.used.

// CHECK-DAG: declare {{.*}}i32 @QueryInterface(ptr, ptr, ptr)
// CHECK-DAG: declare {{.*}}i32 @AddRef(ptr)
// CHECK-DAG: declare {{.*}}i32 @Release(ptr)
// CHECK-DAG: {{.*}}WidgetCMn.com.vtable{{.*}} = private constant {{.*}} ptr @QueryInterface, ptr @AddRef, ptr @Release
