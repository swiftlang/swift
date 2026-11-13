// RUN: %target-swift-frontend -emit-ir %s -module-name A | %FileCheck %s -DINT=i%target-ptrsize

struct Foo {}

struct OneArg<T> {}

func genericMetatype<A>(_ x: A.Type) {}

func makeGenericMetatypes() {
  genericMetatype(OneArg<Foo>.self)
}

public protocol P1 {
  func normal()
}
public protocol P2 {}

public struct IsP2: P2 {}


public struct Single<A> {}

extension Single: P1 where A: P2 {
  public func normal() {}
}

public func takes_p1<T: P1>(_: T.Type) {}

public func single_concrete() {
  takes_p1(Single<IsP2>.self)
}

// The "abstract" call path will mask the LSB in the cache.

// CHECK: define linkonce_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledNameV2(ptr %0, ptr %1)
// CHECK: entry:
// CHECK:   [[T2:%.*]] = load atomic ptr, ptr %0 monotonic
// CHECK:   [[T3:%.*]] = icmp eq ptr [[T2]], null
// CHECK:   [[T4:%.*]] = ptrtoint ptr [[T2]] to [[INT]]
// CHECK:   [[T5:%.*]] = and [[INT]] [[T4]], 1
// CHECK:   [[T6:%.*]] = icmp eq [[INT]] [[T5]], 1
// CHECK:   [[T7:%.*]] = or i1 [[T6]], [[T3]]
// CHECK:   br i1 [[T7]], label %[[L1:[0-9]+]], label %[[L2:[0-9]+]]

// CHECK: [[L2]]:
// CHECK:   [[R:%.*]] = phi ptr [ [[T2]], %entry ], [ [[T18:%.*]], %[[L1]] ]
// CHECK:   ret ptr [[R]]

// CHECK: [[L1]]:
// CHECK:   [[T18]] = call swiftcc ptr @swift_getTypeByMangledNameInContext{{2?}}(
// CHECK:   store atomic ptr %18, ptr %0 monotonic
// CHECK:   br label %8
// CHECK: }


// CHECK: define linkonce_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledNameAbstractV2(ptr %0, ptr %1)
// CHECK: entry:
// CHECK:  [[T2:%.*]] = load atomic ptr, ptr %0 monotonic
// CHECK:  [[T3:%.*]] = icmp eq ptr [[T2]], null
// CHECK:  br i1 [[T3]], label %[[L1:[0-9]+]], label %[[L2:[0-9]+]]

// CHECK:[[L2]]:
// CHECK:  [[T5:%.*]] = phi ptr [ [[T2]], %entry ], [ [[T20:%.*]], %[[L1]] ]
// CHECK:  [[T6:%.*]] = ptrtoint ptr [[T5]] to [[INT]]
// CHECK:  [[T7:%.*]] = and [[INT]] [[T6]], -2
// CHECK:  [[T8:%.*]] = inttoptr [[INT]] [[T7]] to ptr
// CHECK:  ret ptr %8

// CHECK:[[L1]]:
// CHECK:  [[T17:%.*]] = call swiftcc ptr @swift_getTypeByMangledNameInContextInMetadataState{{2?}}(
// CHECK:  [[T18:%.*]] = ptrtoint ptr [[T17]] to [[INT]]
// CHECK:  [[T19:%.*]] = or [[INT]] [[T18]], 1
// CHECK:  [[T20]] = inttoptr [[INT]] [[T19]] to ptr
// CHECK:  store atomic ptr [[T20]], ptr %0 monotonic
// CHECK:  br label %4
// CHECK:}
