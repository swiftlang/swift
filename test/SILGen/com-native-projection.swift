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

// A concrete native object is explicit projected to the selected interface
// address point. The ordinary reference-existential instruction carries this
// representation change without exposing a source-level pointer operation.

// CHECK-LABEL: sil [ossa] @$s{{.*}}7derivedyAA8IDerived_pAA7CObjectCF
// CHECK:         [[COPY:%.*]] = copy_value %0
// CHECK-NEXT:    [[PROJECTED:%.*]] = init_existential_ref [[COPY]] : $CObject : $CObject, $any IDerived
// CHECK-NEXT:    return [[PROJECTED]]
public func derived(_ value: CObject) -> any IDerived {
  value
}

// CHECK-LABNEL: sil [ossa] @$s{{.*}}9unrelatedyAA10IUnrelated_pAA7CObjectCF
// CHECK:          [[COPY:%.*]] = copy_value %0
// CHECK-NEXT:     [[PROJECTED:%.*]] = init_existential_ref [[COPY]] : $CObject : $CObject, $any IUnrelated
// CHECK-NEXT:     return [[PROJECTED]]
public func unrelated(_ value: CObject) -> any IUnrelated {
  value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}5swifty3COM12ISwiftObject_pAA7CObjectCF
// CHECK:         [[COPY:%.*]] = copy_value %0
// CHECK-NEXT:    [[PROJECTED:%.*]] = init_existential_ref [[COPY]] : $CObject : $CObject, $any ISwiftObject
public func swift(_ value: CObject) -> any ISwiftObject {
  value
}

// Refining an existting COM existential opens its one-word interface pointer
// and erases it again without changing its physical projection.

// CHECK-LABEL: sil [ossa] @$s{{.*}}4castyAA5IBase_pAA8IDerived_pF
// CHECK:         [[OPENED:%.*]] = open_com_existential %0 to $@opened(
// CHECK:         [[COPY:%.*]] = copy_value [[OPENED]]
// CHECK-NEXT:    [[REFINED:%.*]] = init_existential_ref [[COPY]]
// CHECK-NEXT:    return [[REFINED]]
public func cast(_ value: any IDerived) -> any IBase {
  value
}
