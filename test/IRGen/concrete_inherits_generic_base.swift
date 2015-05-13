// RUN: %target-swift-frontend -module-name foo -emit-ir %s | FileCheck %s

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
// CHECK:         [[SUPER:%.*]] = call %swift.type* @_TMaC3foo7Derived()
// CHECK:         call void @swift_initializeSuperclass({{.*}}@_TMdC3foo12SuperDerived{{.*}}, %swift.type* [[SUPER]])
class SuperDerived: Derived {
}

// CHECK-LABEL: define %swift.type* @_TMaC3foo7Derived()
// CHECK:         [[SUPER:%.*]] = call %swift.type* @_TMaGC3foo4BaseSS_()
// CHECK:         call void @swift_initializeSuperclass({{.*}}@_TMdC3foo7Derived{{.*}}, %swift.type* [[SUPER]])

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
