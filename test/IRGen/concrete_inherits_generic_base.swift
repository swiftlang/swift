// RUN: %target-swift-frontend -use-native-super-method -module-name foo -emit-ir %s | FileCheck %s

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
    print("\(self.dynamicType) \(T.self) \(first) \(second)")
  }
}

// CHECK-LABEL: define %swift.type* @_TMaC3foo12SuperDerived()
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @_TMLC3foo12SuperDerived
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     [[METADATA:%.*]] = call %swift.type* @swift_getResilientMetadata(
// CHECK-NEXT:     store %swift.type* [[METADATA]], %swift.type** @_TMLC3foo12SuperDerived
// CHECK-NEXT:     br label %cont
// CHECK:       cont:
// CHECK-NEXT:     [[RESULT:%.*]] = phi %swift.type* [ [[CACHE]], %entry ], [ [[METADATA]], %cacheIsNull ]
// CHECK-NEXT:     ret %swift.type* [[RESULT]]

class SuperDerived: Derived {
}

// CHECK-LABEL: define %swift.type* @_TMaC3foo7Derived()
// CHECK:          [[CACHE:%.*]] = load %swift.type*, %swift.type** @_TMLC3foo7Derived
// CHECK-NEXT:     [[COND:%.*]] = icmp eq %swift.type* [[CACHE]], null
// CHECK-NEXT:     br i1 [[COND]], label %cacheIsNull, label %cont

// CHECK:       cacheIsNull:
// CHECK-NEXT:     [[METADATA:%.*]] = call %swift.type* @swift_getResilientMetadata(
// CHECK-NEXT:     store %swift.type* [[METADATA]], %swift.type** @_TMLC3foo7Derived
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

func presentBase<T>(base: Base<T>) {
  base.present()
}

presentBase(SuperDerived(x: "two"))
presentBase(Derived(x: "two"))
presentBase(Base(x: "two"))
presentBase(Base(x: 2))

// CHECK-LABEL: define private %swift.type* @create_generic_metadata_SuperDerived(%swift.type_pattern*, i8**)
// CHECK:         [[TMP:%.*]] = call %swift.type* @_TMaC3foo7Derived()
// CHECK-NEXT:    [[SUPER:%.*]] = bitcast %swift.type* [[TMP:%.*]] to %objc_class*
// CHECK-NEXT:    [[METADATA:%.*]] = call %swift.type* @swift_allocateGenericClassMetadata(%swift.type_pattern* %0, i8** %1, %objc_class* [[SUPER]])
// CHECK:         call void @swift_initializeSuperclass(%swift.type* [[METADATA]], i1 false)
// CHECK-NEXT:    ret %swift.type* [[METADATA]]
