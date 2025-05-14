// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/other_class.swiftmodule %S/Inputs/other_class.swift
// RUN: %target-swift-frontend -I %t -emit-ir -O -enforce-exclusivity=unchecked %s | %FileCheck %s -DINT=i%target-ptrsize

// REQUIRES: PTRSIZE=64

import other_class

// CHECK-LABEL: define {{(protected )?}}{{(dllexport )?}}swiftcc i32 @"$s24class_field_other_module12getSubclassXys5Int32V0c1_A00F0CF"(ptr{{( nocapture)?}} readonly{{( captures\(none\))?}} %0)
// CHECK-NEXT: entry:
// An Int32 after the heap object header
// CHECK-NEXT: [[GEP:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 16
// CHECK-NEXT: [[RESULT:%.*]] = load i32, ptr [[GEP]]
// CHECK-NEXT: ret i32 [[RESULT]]
public func getSubclassX(_ o: Subclass) -> Int32 {
  return o.x
}

// CHECK-LABEL: define {{(protected )?}}{{(dllexport )?}}swiftcc i32 @"$s24class_field_other_module12getSubclassYys5Int32V0c1_A00F0CF"(ptr{{( nocapture)?}} readonly{{( captures\(none\))?}} %0)
// CHECK-NEXT: entry:
// An Int32 after an Int32 after the heap object header
// CHECK-NEXT: [[GEP:%.*]] = getelementptr inbounds{{.*}} i8, ptr %0, i64 20
// CHECK-NEXT: [[RESULT:%.*]] = load i32, ptr [[GEP]]
// CHECK-NEXT: ret i32 [[RESULT]]
public func getSubclassY(_ o: Subclass) -> Int32 {
  return o.y
}
