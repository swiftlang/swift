// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -module-name foo -emit-ir %s | %FileCheck %s

// CHECK: %swift.type = type { [[INT:i32|i64]] }

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

// CHECK-LABEL: define hidden %swift.type* @_T03foo12SuperDerivedCMa()
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @_T03foo12SuperDerivedCML
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     call void @swift_once([[INT]]* @_T03foo12SuperDerivedCMa.once_token, i8* bitcast (void (i8*)* @initialize_metadata_SuperDerived to i8*))
// CHECK-NEXT:     [[METADATA:%.*]] = load %swift.type*, %swift.type** @_T03foo12SuperDerivedCML
// CHECK-NEXT:     br label %cont
// CHECK:       cont:
// CHECK-NEXT:     [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     ret %swift.type* [[RESULT]]

class SuperDerived: Derived {
}

// CHECK-LABEL: define hidden %swift.type* @_T03foo7DerivedCMa()
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @_T03foo7DerivedCML
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     call void @swift_once([[INT]]* @_T03foo7DerivedCMa.once_token, i8* bitcast (void (i8*)* @initialize_metadata_Derived to i8*))
// CHECK-NEXT:     [[METADATA:%.*]] = load %swift.type*, %swift.type** @_T03foo7DerivedCML
// CHECK-NEXT:     br label %cont
// CHECK:       cont:
// CHECK-NEXT:     [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     ret %swift.type* [[RESULT]]

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

// CHECK-LABEL: define{{( protected)?}} private void @initialize_metadata_SuperDerived(i8*)
// CHECK:         [[TMP:%.*]] = call %swift.type* @_T03foo7DerivedCMa()
// CHECK-NEXT:    store %swift.type* [[TMP]], %swift.type** getelementptr inbounds ({{.*}} @_T03foo12SuperDerivedCMf{{.*}}, i32 1), align
// CHECK:         [[METADATA:%.*]] = call %swift.type* @swift_initClassMetadata_UniversalStrategy(
// CHECK:         store atomic %swift.type* [[METADATA]], %swift.type** @_T03foo12SuperDerivedCML release,
// CHECK:         ret void
