// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple | %FileCheck %s

protocol P {
  associatedtype A
  var a: A { get }
}

func f<each T: P>(_ t: repeat each T) -> (repeat (each T).A) {
  let data = (repeat (each t).a)
  return data
}

// CHECK-LABEL: define {{.*}} void @"$s21pack_metadata_dealloc1fy1AQzxQp_txxQpRvzAA1PRzlF"
// CHECK: [[SPSAVE:%.*]] = call ptr @llvm.stacksave.p0()
// CHECK: call void @llvm.stackrestore.p0(ptr [[SPSAVE]])
// CHECK: [[SPSAVE1:%.*]] = call ptr @llvm.stacksave.p0()
// CHECK: [[SPSAVE2:%.*]] = call ptr @llvm.stacksave.p0()
// CHECK-NOT: call ptr llvm.stacksave.p0()
// CHECK:  call void @llvm.stackrestore.p0(ptr [[SPSAVE2]])
// CHECK:  call void @llvm.stackrestore.p0(ptr [[SPSAVE1]])
// CHECK:  ret void

struct G<each T> {
  init(_: repeat each T) {}
}

func f2<each T: P, each U>(t: repeat each T, u: repeat each U) async -> (repeat G<(each T).A, repeat each U>) {
  let x = (repeat G((each t).a, repeat each u))
  return x
}