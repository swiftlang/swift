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
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @"$S3foo12SuperDerivedCML"
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     call void @swift_once([[INT]]* @"$S3foo12SuperDerivedCMa.once_token", i8* bitcast (void (i8*)* @initialize_metadata_SuperDerived to i8*), i8* undef)
// CHECK-NEXT:     [[METADATA:%.*]] = load %swift.type*, %swift.type** @"$S3foo12SuperDerivedCML"
// CHECK-NEXT:     br label %cont
// CHECK:       cont:
// CHECK-NEXT:     [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[RESULT]], 0
// CHECK-NEXT:     [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] 0, 1
// CHECK-NEXT:     ret %swift.metadata_response [[T1]]

class SuperDerived: Derived {
}

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S3foo7DerivedCMa"(
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @"$S3foo7DerivedCML"
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     call void @swift_once([[INT]]* @"$S3foo7DerivedCMa.once_token", i8* bitcast (void (i8*)* @initialize_metadata_Derived to i8*), i8* undef)
// CHECK-NEXT:     [[METADATA:%.*]] = load %swift.type*, %swift.type** @"$S3foo7DerivedCML"
// CHECK-NEXT:     br label %cont
// CHECK:       cont:
// CHECK-NEXT:     [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     [[T0:%.*]] = insertvalue %swift.metadata_response undef, %swift.type* [[RESULT]], 0
// CHECK-NEXT:     [[T1:%.*]] = insertvalue %swift.metadata_response [[T0]], [[INT]] 0, 1
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

// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} private void @initialize_metadata_SuperDerived(i8*)
// CHECK:         [[T0:%.*]] = call swiftcc %swift.metadata_response @"$S3foo7DerivedCMa"([[INT]] 1)
// CHECK:         [[TMP:%.*]] = extractvalue %swift.metadata_response [[T0]], 0
// CHECK-NEXT:    extractvalue %swift.metadata_response [[T0]], 1
// CHECK-NEXT:    store %swift.type* [[TMP]], %swift.type** getelementptr inbounds ({{.*}} @"$S3foo12SuperDerivedCMf{{.*}}, i32 1), align
// CHECK:         [[METADATA:%.*]] = call %swift.type* @swift_relocateClassMetadata({{.*}}, [[INT]] {{60|96}}, [[INT]] 0)
// CHECK:         call void @swift_initClassMetadata(%swift.type* [[METADATA]], [[INT]] 0, {{.*}})
// CHECK:         store atomic %swift.type* [[METADATA]], %swift.type** @"$S3foo12SuperDerivedCML" release,
// CHECK:         ret void
