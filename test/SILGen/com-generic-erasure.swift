// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-silgen %s | %FileCheck %s

@com(interface: "30000000-0000-0000-0000-000000000001")
public protocol IBase { }

@com(interface: "30000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase { }

@com(interface: "30000000-0000-0000-0000-000000000003")
public protocol IUnrelated { }

@com
public final class CObject: IDerived, IUnrelated {
}

// A COM-constrained generic value keeps its ordinary Swift representation.

// CHECK-LABEL: sil [ossa] @$s{{.*}}genericyxxAA8IDerivedRzlF
// CHECK-SAME:  <Derived where Derived : IDerived> (@in_guaranteed Derived) -> @out Derived
// CHECK:         bb0([[RESULT:%.*]] : $*Derived, [[VALUE:%.*]] : $*Derived
// CHECK:         copy_addr [[VALUE]] to [init] [[RESULT]]
public func generic<Derived: IDerived>(_ value: Derived) -> Derived {
  value
}

// Only existential erasure requests the interface projection.

// CHECK-LABEL: sil [ossa] @$s{{.*}}6erasedyAA8IDerived_pxAaCRzlF
// CHECK-SAME:  <Derived where Derived : IDerived> (@in_guaranteed Derived) -> @owned any IDerived
// CHECK:         bb0([[VALUE:%.*]] : $*Derived):
// CHECK:         [[COPY:%.*]] = alloc_stack $Derived
// CHECK:         copy_addr [[VALUE]] to [init] [[COPY]]
// CHECK:         [[ERASED:%.*]] = init_existential_ref [[COPY]] : $*Derived : $Derived, $any IDerived, forwarding: @owned
// CHECK:         destroy_addr [[COPY]]
// CHECK:         return [[ERASED]]
public func erased<Derived: IDerived>(_ value: Derived) -> any IDerived {
  value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}6castedyAA5IBase_pxAA8IDerivedRzlF
// CHECK:         [[ERASED:%.*]] = init_existential_ref {{%.*}} : $*Derived : $Derived, $any IBase, forwarding: @owned
public func casted<Derived: IDerived>(_ value: Derived) -> any IBase {
  value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}9unrelatedyAA10IUnrelated_pxAaCRzlF
// CHECK:         [[ERASED:%.*]] = init_existential_ref {{%.*}} : $*Unrelated : $Unrelated, $any IUnrelated, forwarding: @owned
public func unrelated<Unrelated: IUnrelated>(_ value: Unrelated) -> any IUnrelated {
  value
}

// Nested generic forwarding continues to pass T itself, its metadata, and the
// witness used for interface erasure.

// CHECK-LABEL: sil [ossa] @$s{{.*}}7forwardyAA8IDerived_pxAaCRzlF
// CHECK:         apply {{%.*}}<Derived>({{%.*}}, %0)
// CHECK:         apply {{%.*}}<Derived>({{%.*}})
public func forward<Derived: IDerived>(_ value: Derived) -> any IDerived {
  erased(generic(value))
}
