// RUN: %target-swift-frontend -enable-experimental-feature Lifetimes -O -disable-llvm-optzns -disable-availability-checking -emit-ir -module-name main %s | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

public struct BigPod { var a, b, c, d, e: Int }
public struct BigArc { var a, b, c, d, e: AnyObject }

@_silgen_name("makeBigPod")
func makeBigPod() -> BigPod

@_silgen_name("makeBigArc")
func makeBigArc() -> BigArc


// CHECK-LABEL: define {{.*}} @"$s{{.*}}12borrowBigPod
// CHECK:         ret ptr %0
@_lifetime(borrow target)
public func borrowBigPod(_ target: BigPod) -> Ref<BigPod> {
	return Ref(target)
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}12borrowBigArc
// CHECK:         [[TMP:%.*]] = getelementptr inbounds nuw %T{{.*}}6BigArcVG, ptr [[BUF:%.*]], i32 0, i32 0
// CHECK:         store ptr %0, ptr [[TMP:%.*]], align
// CHECK:         [[TMP:%.*]] = getelementptr inbounds nuw %T{{.*}}6BigArcVG, ptr [[BUF]], i32 0, i32 0
// CHECK:         [[RESULT:%.*]] = load ptr, ptr [[TMP]]
// CHECK:         ret ptr [[RESULT]]
@_lifetime(borrow target)
public func borrowBigArc(_ target: BigArc) -> Ref<BigArc> {
	return Ref(target)
}

@_silgen_name("takeBorrowBigPod")
func takeBorrowBigPod(_: Ref<BigPod>)

// CHECK-LABEL: define {{.*}} @"$s{{.*}}17borrowBigPodLocal
// CHECK:         call {{.*}} @makeBigPod(ptr noalias sret(%T4main6BigPodV) captures(none) [[LOCAL_POD:%.*]])
// CHECK:         call {{.*}} @takeBorrowBigPod(ptr [[LOCAL_POD]])
public func borrowBigPodLocal() {
	let target = makeBigPod()
	takeBorrowBigPod(Ref(target))
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}11derefBigPod
public func derefBigPod(_ borrow: Ref<BigPod>) -> BigPod {
	// CHECK:     store ptr %1, ptr [[TMP:%.*]], align
	// CHECK:     [[BORROW:%.*]] = load ptr, ptr [[TMP]]
	// copying the value from the borrow target to the return buffer:
    // CHECK:     [[BORROW_A:%.*]] = getelementptr inbounds nuw %T4main6BigPodV, ptr [[BORROW]], i32 0, i32 0
    // CHECK:     [[BORROW_A_VALUE:%.*]] = getelementptr inbounds nuw %TSi, ptr [[BORROW_A]], i32 0, i32 0
    // CHECK:     [[A_VALUE:%.*]] = load [[WORD:i[0-9]+]], ptr [[BORROW_A_VALUE]], align
    // CHECK:     [[RESULT_A:%.*]] = getelementptr inbounds nuw %T4main6BigPodV, ptr %0, i32 0, i32 0
    // CHECK:     [[RESULT_A_VALUE:%.*]] = getelementptr inbounds nuw %TSi, ptr [[RESULT_A]], i32 0, i32 0
    // CHECK:     store [[WORD]] [[A_VALUE]], ptr [[RESULT_A_VALUE]], align
	return borrow.value
}

public func derefBigArc(_ borrow: Ref<BigArc>) -> BigArc {
	return borrow.value
}
