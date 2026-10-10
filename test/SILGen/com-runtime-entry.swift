// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Sema/Inputs/com-runtime-entry.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=corefoundation -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IWidget { }

@com
final class Widget: IWidget { }

// Canonical runtime definitions are represented by external SIL declarations.

// CHECK-DAG: sil {{.*}}[asmname "QueryInterface"] {{.*}} : $@convention(c)
// CHECK-DAG: sil {{.*}}[asmname "AddRef"] {{.*}} : $@convention(c)
// CHECK-DAG: sil {{.*}}[asmname "Release"] {{.*}} : $@convention(c)
