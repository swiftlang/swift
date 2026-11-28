// RUN: %target-swift-frontend -emit-ir -O -module-name test -enable-library-evolution -enable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors %s | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: swift_feature_CoroutineAccessors

// A call through an async or coroutine function pointer branches to an address
// computed from the record that the pointer refers to. The address is loaded
// from the record already signed, so the call is still authenticated after
// optimization.

// CHECK-LABEL: define {{.*}} @"$s4test10viaClosureyS2iyYaXEYaF"(
// CHECK:         [[FN:%.*]] = tail call i64 @llvm.ptrauth.resign.load.relative(i64 %{{.*}}, i32 2, i64 [[DISC:[0-9]+]], i32 0, i64 [[DISC]], i64 0)
// CHECK:         [[FNPTR:%.*]] = inttoptr i64 [[FN]] to ptr
// CHECK:         musttail call swifttailcc void [[FNPTR]]({{.*}}) {{.*}}[ "ptrauth"(i32 0, i64 [[DISC]]) ]
public func viaClosure(_ f: () async -> Int) async -> Int { await f() }

public protocol P {
  func m() async -> Int
}

// CHECK-LABEL: define {{.*}} @"$s4test10viaWitnessySixYaAA1PRzlF"(
// CHECK:         [[FN:%.*]] = tail call i64 @llvm.ptrauth.resign.load.relative(i64 %{{.*}}, i32 2, i64 [[DISC:%.*]], i32 0, i64 [[DISC]], i64 0)
// CHECK:         [[FNPTR:%.*]] = inttoptr i64 [[FN]] to ptr
// CHECK:         musttail call swifttailcc void [[FNPTR]]({{.*}}) {{.*}}[ "ptrauth"(i32 0, i64 [[DISC]]) ]
public func viaWitness<T: P>(_ t: T) async -> Int { await t.m() }

open class C {
  open func m() async -> Int { 1 }
}

// CHECK-LABEL: define {{.*}} @"$s4test8viaClassySiAA1CCYaF"(
// CHECK:         [[FN:%.*]] = tail call i64 @llvm.ptrauth.resign.load.relative(i64 %{{.*}}, i32 2, i64 [[DISC:%.*]], i32 0, i64 [[DISC]], i64 0)
// CHECK:         [[FNPTR:%.*]] = inttoptr i64 [[FN]] to ptr
// CHECK:         musttail call swifttailcc void [[FNPTR]]({{.*}}) {{.*}}[ "ptrauth"(i32 0, i64 [[DISC]]) ]
public func viaClass(_ c: C) async -> Int { await c.m() }

public protocol Q {
  var v: Int { yielding borrow }
}

// CHECK-LABEL: define {{.*}} @"$s4test5readVySixAA1QRzlF"(
// CHECK:         [[FN:%.*]] = tail call i64 @llvm.ptrauth.resign.load.relative(i64 %{{.*}}, i32 2, i64 [[DISC:%.*]], i32 0, i64 [[DISC]], i64 0)
// CHECK:         [[FNPTR:%.*]] = inttoptr i64 [[FN]] to ptr
// CHECK:         call swiftcc { ptr, i64 } [[FNPTR]]({{.*}}) [ "ptrauth"(i32 0, i64 [[DISC]]) ]
public func readV<T: Q>(_ t: T) -> Int { t.v }
