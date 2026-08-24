// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -disable-llvm-optzns -I %t -emit-ir %s -o - | %FileCheck %s

@com(interface: "47000000-0000-0000-0000-000000000001")
public protocol IWidget {
}

@inline(never)
public func modify<T: IWidget>(_ value: inout T) {
}

// The opened value occupies the existential's one-word storage, so IRGen
// passes its address directly and uses the identity interface adjustment.

// CHECK-LABEL: define{{.*}} swiftcc void @"$s{{.*}}15openExistential
// CHECK-SAME:  (ptr {{.*}} [[VALUE:%[^,)]+]])
// CHECK-NOT:     getelementptr
// CHECK-NOT:     load ptr
// CHECK:         call swiftcc void @"$s{{.*}}6modify{{.*}}"(ptr {{.*}} [[VALUE]], ptr {{.*}}, i{{32|64}} 0)
// CHECK:         ret void
public func openExistential(_ value: inout any IWidget) {
  modify(&value)
}
