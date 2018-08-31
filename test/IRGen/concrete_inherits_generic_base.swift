// RUN: %target-swift-frontend -module-name foo -emit-ir %s | %FileCheck %s -DINT=i%target-ptrsize

// CHECK: %swift.type = type { [[INT]] }

// -- Classes with generic bases can't go in the @objc_classes list, since
//    they need runtime initialization before they're valid.
// CHECK-NOT: @objc_classes

class Base<T> {
  var first, second: T

  required init(x: T) {
    first = x
    second = x
  }

  func present() {
    print("\(type(of: self)) \(T.self) \(first) \(second)")
  }
}

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S3foo12SuperDerivedCMa"(
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** getelementptr inbounds ({ %swift.type*, i8* }, { %swift.type*, i8* }* @"$S3foo12SuperDerivedCMl", i32 0, i32 0)
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S3foo12SuperDerivedCMn" to %swift.type_descriptor*))
// CHECK-NEXT:     [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT:     [[STATUS:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 1
// CHECK-NEXT:     br label %cont

// CHECK:       cont:
// CHECK-NEXT:     [[NEW_METADATA:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     [[NEW_STATUS:%.*]] = phi [[INT]] [ 0, %entry ], [ [[STATUS]], %cacheIsNull ]
// CHECK-NEXT:     [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[NEW_METADATA]], 0
// CHECK-NEXT:     [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT:     ret %swift.metadata_response [[T1]]

class SuperDerived: Derived {
}

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S3foo7DerivedCMa"(
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** getelementptr inbounds ({ %swift.type*, i8* }, { %swift.type*, i8* }* @"$S3foo7DerivedCMl", i32 0, i32 0)
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     [[RESPONSE:%.*]] = call swiftcc %swift.metadata_response @swift_getSingletonMetadata([[INT]] %0, %swift.type_descriptor* bitcast ({{.*}} @"$S3foo7DerivedCMn" to %swift.type_descriptor*))
// CHECK-NEXT:     [[METADATA:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 0
// CHECK-NEXT:     [[STATUS:%.*]] = extractvalue %swift.metadata_response [[RESPONSE]], 1
// CHECK-NEXT:     br label %cont

// CHECK:       cont:
// CHECK-NEXT:     [[NEW_METADATA:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     [[NEW_STATUS:%.*]] = phi [[INT]] [ 0, %entry ], [ [[STATUS]], %cacheIsNull ]
// CHECK-NEXT:     [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[NEW_METADATA]], 0
// CHECK-NEXT:     [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] [[NEW_STATUS]], 1
// CHECK-NEXT:     ret %swift.metadata_response [[T1]]

class Derived: Base<String> {
  var third: String

  required init(x: String) {
    third = x
    super.init(x: x)
  }

  override func present() {
    super.present()
    print("...and \(third)")
  }
}

func presentBase<T>(_ base: Base<T>) {
  base.present()
}

presentBase(SuperDerived(x: "two"))
presentBase(Derived(x: "two"))
presentBase(Base(x: "two"))
presentBase(Base(x: 2))

// CHECK-LABEL: define internal swiftcc %swift.metadata_response @"$S3foo12SuperDerivedCMr"(%swift.type*, i8*, i8**)
// CHECK:         [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S3foo7DerivedCMa"([[INT]] 257)
// CHECK-NEXT:    [[SUPERCLASS:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    [[STATUS:%.*]] = extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT:    [[RESULT:%.*]] = icmp ule [[INT]] [[STATUS]], 1
// CHECK-NEXT:    br i1 [[RESULT]], label %dependency-satisfied, label %metadata-dependencies.cont

// CHECK: dependency-satisfied:

// -- ClassLayoutFlags = 0x100 (HasStaticVTable)
// CHECK:         call void @swift_initClassMetadata(%swift.type* %0, %swift.type* [[SUPERCLASS]], [[INT]] 256, {{.*}})

// CHECK: metadata-dependencies.cont:
// CHECK:         ret %swift.metadata_response
