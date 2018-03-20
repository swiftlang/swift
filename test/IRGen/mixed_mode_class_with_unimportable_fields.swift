// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/UsingObjCStuff.swiftmodule -module-name UsingObjCStuff -I %t -I %S/Inputs/mixed_mode -swift-version 4 %S/Inputs/mixed_mode/UsingObjCStuff.swift
// RUN: %target-swift-frontend -emit-ir -I %t -I %S/Inputs/mixed_mode -module-name main -swift-version 3 %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-V3
// RUN: %target-swift-frontend -emit-ir -I %t -I %S/Inputs/mixed_mode -module-name main -swift-version 4 %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-V4

// REQUIRES: objc_interop

import UsingObjCStuff

public class SubButtHolder: ButtHolder {
  final var w: Double = 0

  override public func virtual() {}

  public func subVirtual() {}
}

public class SubSubButtHolder: SubButtHolder {
  public override func virtual() {}
  public override func subVirtual() {}
}

// CHECK-LABEL: define {{.*}} @{{.*}}accessFinalFields
public func accessFinalFields(of holder: ButtHolder) -> (Any, Any) {
  // x and z came from the other module, so we should use their accessors.
  // CHECK: [[OFFSET:%.*]] = load [[WORD:i[0-9]+]], [[WORD]]* @"$S14UsingObjCStuff10ButtHolderC1xSivpWvd"
  // CHECK: [[INSTANCE_RAW:%.*]] = bitcast {{.*}} to i8*
  // CHECK: getelementptr inbounds i8, i8* [[INSTANCE_RAW]], [[WORD]] [[OFFSET]]

  // CHECK: [[OFFSET:%.*]] = load [[WORD]], [[WORD]]* @"$S14UsingObjCStuff10ButtHolderC1zSSvpWvd"
  // CHECK: [[INSTANCE_RAW:%.*]] = bitcast {{.*}} to i8*
  // CHECK: getelementptr inbounds i8, i8* [[INSTANCE_RAW]], [[WORD]] [[OFFSET]]
  return (holder.x, holder.z)
}

// CHECK-LABEL: define {{.*}} @{{.*}}accessFinalFields
public func accessFinalFields(ofSub holder: SubButtHolder) -> (Any, Any, Any) {
  // We should use the runtime-adjusted ivar offsets since we may not have
  // a full picture of the layout in mixed Swift language version modes.
  // CHECK: [[OFFSET:%.*]] = load [[WORD]], [[WORD]]* @"$S14UsingObjCStuff10ButtHolderC1xSivpWvd"
  // CHECK: [[INSTANCE_RAW:%.*]] = bitcast {{.*}} to i8*
  // CHECK: getelementptr inbounds i8, i8* [[INSTANCE_RAW]], [[WORD]] [[OFFSET]]

  // CHECK: [[OFFSET:%.*]] = load [[WORD]], [[WORD]]* @"$S14UsingObjCStuff10ButtHolderC1zSSvpWvd"
  // CHECK: [[INSTANCE_RAW:%.*]] = bitcast {{.*}} to i8*
  // CHECK: getelementptr inbounds i8, i8* [[INSTANCE_RAW]], [[WORD]] [[OFFSET]]

  // CHECK: [[OFFSET:%.*]] = load [[WORD]], [[WORD]]* @"$S4main13SubButtHolderC1wSdvpWvd"

  // CHECK: [[INSTANCE_RAW:%.*]] = bitcast {{.*}} to i8*
  // CHECK: getelementptr inbounds i8, i8* [[INSTANCE_RAW]], [[WORD]] [[OFFSET]]
  return (holder.x, holder.z, holder.w)
}

// CHECK-LABEL: define {{.*}} @{{.*}}invokeMethod
public func invokeMethod(on holder: SubButtHolder) {
  // CHECK-64: [[IMPL_ADDR:%.*]] = getelementptr inbounds {{.*}}, [[WORD]] 10
  // CHECK-32: [[IMPL_ADDR:%.*]] = getelementptr inbounds {{.*}}, [[WORD]] 13
  // CHECK: [[IMPL:%.*]] = load {{.*}} [[IMPL_ADDR]]
  // CHECK: call swiftcc void [[IMPL]]
  holder.virtual()
  // CHECK-64: [[IMPL_ADDR:%.*]] = getelementptr inbounds {{.*}}, [[WORD]] 15
  // CHECK-32: [[IMPL_ADDR:%.*]] = getelementptr inbounds {{.*}}, [[WORD]] 18
  // CHECK: [[IMPL:%.*]] = load {{.*}} [[IMPL_ADDR]]
  // CHECK: call swiftcc void [[IMPL]]
  holder.subVirtual()
}

// CHECK-V3-LABEL: define private void @initialize_metadata_SubButtHolder
// CHECK-V3:   call void @swift_initClassMetadata(

// CHECK-V3-LABEL: define private void @initialize_metadata_SubSubButtHolder
// CHECK-V3:   call void @swift_initClassMetadata(

