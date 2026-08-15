// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IIndependent {
}

@com
public final class CObject: IDerived, IIndependent {
}

// Projection is a constant address adjustment selected from the class's
// canonical interface layout, and ownership remains a single retain of the
// native object. ISwiftObject occupies the first prefix word, so user
// projections begin at the second word.

public func derived(_ value: CObject) -> any IDerived {
  value
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}7derivedyAA8IDerived_pAA7CObjectCF"
// CHECK-SAME:  (ptr [[OBJECT:%.*]])
// CHECK:         call ptr @swift_retain(ptr returned [[OBJECT]])
// CHECK:         [[INTERFACE:%.*]] = getelementptr inbounds i8, ptr [[OBJECT]], i64 -16
// CHECK-NEXT:    ret ptr [[INTERFACE]]

public func independent(_ value: CObject) -> any IIndependent {
  value
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}11independentyAA12IIndependent_pAA7CObjectCF"
// CHECK-SAME:  (ptr [[OBJECT:%.*]])
// CHECK:         call ptr @swift_retain(ptr returned [[OBJECT]])
// CHECK:         [[INTERFACE:%.*]] = getelementptr inbounds i8, ptr [[OBJECT]], i64 -24
// CHECK-NEXT:    ret ptr [[INTERFACE]]

#if $_MicrosoftCOM

// IUnknown always uses the primary user projection. ISwiftObject is an
// independent compiler-managed interface.

public func unknown(_ value: CObject) -> any IUnknown {
  value
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}7unknowny3COM8IUnknown_pAA7CObjectCF"
// CHECK-SAME:  (ptr [[OBJECT:%.*]])
// CHECK:         call ptr @swift_retain(ptr returned [[OBJECT]])
// CHECK:         [[INTERFACE:%.*]] = getelementptr inbounds i8, ptr [[OBJECT]], i64 -16
// CHECK-NEXT:    ret ptr [[INTERFACE]]

#endif

// ISwiftObject uses the dedicated projection closest to the Swift object.

public func swift(_ value: CObject) -> any ISwiftObject {
  value
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}5swifty3COM12ISwiftObject_pAA7CObjectCF"
// CHECK-SAME:  (ptr [[OBJECT:%.*]])
// CHECK:         call ptr @swift_retain(ptr returned [[OBJECT]])
// CHECK:         [[INTERFACE:%.*]] = getelementptr inbounds i8, ptr [[OBJECT]], i64 -8
// CHECK-NEXT:    ret ptr [[INTERFACE]]

// A base of the current interface's refinement chain has the same address
// point. Its copy is retained through AddRef, without another adjustment.

public func refine(_ value: any IDerived) -> any IBase {
  value
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6refineyAA5IBase_pAA8IDerived_pF"
// CHECK-SAME:  (ptr [[INTERFACE:%.*]])
// CHECK-NOT:     getelementptr inbounds i8, ptr [[INTERFACE]]
// CHECK:         call i32 {{%.*}}(ptr [[INTERFACE]])
// CHECK-NOT:     getelementptr inbounds i8, ptr [[INTERFACE]]
// CHECK:         ret ptr [[INTERFACE]]
