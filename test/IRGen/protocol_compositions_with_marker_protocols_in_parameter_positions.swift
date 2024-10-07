// RUN: %target-swift-frontend -module-name marker_test -emit-ir %s -swift-version 5  -disable-availability-checking | %IRGenFileCheck %s

// REQUIRES: concurrency

public protocol P<Output> {
  associatedtype Output
}

struct Aux {
}

extension Aux {
  public struct Test<Root : P, Output> : P {
    public let root: Root
    public let keyPath: KeyPath<Root.Output, Output> & Sendable

    // CHECK-LABEL: define hidden swiftcc void @"$s11marker_test3AuxV4TestV4root7keyPathAEy_xq_Gx_s8Sendable_s03KeyG0Cy6OutputQzq_GXctcfC"(ptr noalias sret(%swift.opaque) %0, ptr noalias %1, ptr %2, ptr %Root, ptr %Root.P)
    // CHECK: [[OUTPUT_ADDR:%.*]] = getelementptr inbounds i8, ptr %3, i64 {{.*}}
    // CHECK-NEXT: %Output = load ptr, ptr [[OUTPUT_ADDR]]
    // CHECK-NEXT: store ptr %Output, ptr %Output2
    // CHECK-NEXT: call swiftcc %swift.metadata_response @"$s11marker_test3AuxV4TestVMa"(i64 0, ptr %Root, ptr %Output, ptr %Root.P)
    public init(root: Root, keyPath: Swift.KeyPath<Root.Output, Output> & Sendable) {
      self.root = root
      self.keyPath = keyPath
    }
  }

  // ! - Note that build<Root, Value> and build_no_sendable<Root, Value> should be the same.

  // CHECK-LABEL: define hidden swiftcc void @"$s11marker_test3AuxV5build4root7keyPathAC4TestVy_xq_Gx_s03KeyG0Cy6OutputQzq_GtAA1PRzr0_lFZ"(ptr noalias sret(%swift.opaque) %0, ptr noalias %1, ptr %2, ptr %Root, ptr %Root.P)
  // CHECK: [[WTABLE_ADDR:%.*]] = getelementptr inbounds ptr, ptr %Root, i64 -1
  // CHECK-NEXT: %Root.valueWitnesses = load ptr, ptr [[WTABLE_ADDR]]
  // CHECK: [[WTABLE_SIZE:%.*]] = getelementptr inbounds %swift.vwtable, ptr %Root.valueWitnesses, i32 0, i32 8
  // CHECK-NEXT: %size = load i64, ptr [[WTABLE_SIZE]]
  // CHECK-NEXT: [[WTABLE:%.*]] = alloca i8, i64 %size
  // CHECK: %base = load i64, ptr @"$ss7KeyPathCMo"
  // CHECK-NEXT: [[KEYPATH_BASE_OFFSET:%.*]] = add i64 %base, 0
  // CHECK-NEXT: [[OUTPUT_ADDR:%.*]] = getelementptr inbounds i8, ptr %3, i64 [[KEYPATH_BASE_OFFSET]]
  // CHECK-NEXT: %Root.Output = load ptr, ptr [[OUTPUT_ADDR]]
  // CHECK: call swiftcc void @"$s11marker_test3AuxV4TestV4root7keyPathAEy_xq_Gx_s8Sendable_s03KeyG0Cy6OutputQzq_GXctcfC"(ptr noalias sret(%swift.opaque) %0, ptr noalias [[WTABLE]], ptr %2, ptr %Root, ptr %Root.P)
  @preconcurrency
  public static func build<Root, Value>(root: Root, keyPath: KeyPath<Root.Output, Value> & Sendable) -> Aux.Test<Root, Value> {
    Test(root: root, keyPath: keyPath)
  }

  // CHECK-LABEL: define hidden swiftcc void @"$s11marker_test3AuxV17build_no_sendable4root7keyPathAC4TestVy_xq_Gx_s8Sendable_s03KeyI0Cy6OutputQzq_GXctAA1PRzr0_lFZ"(ptr noalias sret(%swift.opaque) %0, ptr noalias %1, ptr %2, ptr %Root, ptr %Root.P)
  // CHECK: [[WTABLE_ADDR:%.*]] = getelementptr inbounds ptr, ptr %Root, i64 -1
  // CHECK-NEXT: %Root.valueWitnesses = load ptr, ptr [[WTABLE_ADDR]]
  // CHECK: [[WTABLE_SIZE:%.*]] = getelementptr inbounds %swift.vwtable, ptr %Root.valueWitnesses, i32 0, i32 8
  // CHECK-NEXT: %size = load i64, ptr [[WTABLE_SIZE]]
  // CHECK-NEXT: [[WTABLE:%.*]] = alloca i8, i64 %size
  // CHECK: %base = load i64, ptr @"$ss7KeyPathCMo"
  // CHECK-NEXT: [[KEYPATH_BASE_OFFSET:%.*]] = add i64 %base, 0
  // CHECK-NEXT: [[OUTPUT_ADDR:%.*]] = getelementptr inbounds i8, ptr %3, i64 [[KEYPATH_BASE_OFFSET]]
  // CHECK-NEXT: %Root.Output = load ptr, ptr [[OUTPUT_ADDR]]
  // CHECK: call swiftcc void @"$s11marker_test3AuxV4TestV4root7keyPathAEy_xq_Gx_s8Sendable_s03KeyG0Cy6OutputQzq_GXctcfC"(ptr noalias sret(%swift.opaque) %0, ptr noalias [[WTABLE]], ptr %2, ptr %Root, ptr %Root.P)
  public static func build_no_sendable<Root, Value>(root: Root, keyPath: KeyPath<Root.Output, Value> & Sendable) -> Aux.Test<Root, Value> {
    Test(root: root, keyPath: keyPath)
  }
}
