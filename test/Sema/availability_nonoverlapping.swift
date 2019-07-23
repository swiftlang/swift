// RUN: not %target-swift-frontend -typecheck %s -swift-version 4 2> %t.4.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-4 %s < %t.4.txt
// RUN: %FileCheck -check-prefix=NEGATIVE -check-prefix=NEGATIVE-4 %s < %t.4.txt

// RUN: not %target-swift-frontend -typecheck %s -swift-version 5 2> %t.5.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=CHECK-5 %s < %t.5.txt
// RUN: %FileCheck -check-prefix=NEGATIVE -check-prefix=NEGATIVE-5 %s < %t.5.txt


class NonOptToOpt {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = NonOptToOpt()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0

class NonOptToOptReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = NonOptToOptReversed()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0


class OptToNonOpt {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init!() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = OptToNonOpt()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0

class OptToNonOptReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init!() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = OptToNonOptReversed()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0


class NoChange {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {}

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init()'
}

class NoChangeReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {}

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init()'
}

class OptToOpt {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init!() {}

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init?() {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init()'
}

class OptToOptReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init?() {}

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init!() {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init()'
}

class ThreeWayA {
  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class ThreeWayB {
  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class ThreeWayC {
  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class ThreeWayD {
  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class ThreeWayE {
  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class ThreeWayF {
  @available(swift, introduced: 5.0, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 6.0)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class DisjointThreeWay {
  @available(swift, obsoleted: 5.0)
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 5.1, obsoleted: 6.0)
  public init?() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, introduced: 6.1)
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

class OverlappingVersions {
  @available(swift, obsoleted: 6.0)
  public init(a: ()) {}

  @available(swift 5.0)
  public init?(a: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(a:)'

  @available(swift 5.0)
  public init?(b: ()) {}

  @available(swift, obsoleted: 5.1)
  public init(b: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(b:)'

  public init(c: ()) {}

  @available(swift 5.0)
  public init?(c: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(c:)'

  @available(swift 5.0)
  public init(c2: ()) {}

  public init?(c2: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(c2:)'

  @available(swift, obsoleted: 5.0)
  public init(d: ()) {}

  public init?(d: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(d:)'

  public init(d2: ()) {}

  @available(swift, obsoleted: 5.0)
  public init?(d2: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(d2:)'

  @available(swift, obsoleted: 5.0)
  public init(e: ()) {}

  @available(swift 5.0)
  public init?(e: ()) {}

  @available(swift 5.0)
  public init!(e: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(e:)'

  @available(swift, obsoleted: 5.0)
  public init(f: ()) {}

  @available(swift 5.0)
  public init?(f: ()) {}

  @available(swift, obsoleted: 5.0)
  public init!(f: ()) {} // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'init(f:)'
}


class NonThrowingToThrowing {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public static func foo() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public static func foo() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = NonThrowingToThrowing()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0
_ = NonThrowingToThrowing.foo()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0

class NonThrowingToThrowingReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public static func foo() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public static func foo() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = NonThrowingToThrowingReversed()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0
_ = NonThrowingToThrowingReversed.foo()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0


class ThrowingToNonThrowing {
  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public static func foo() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public static func foo() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = ThrowingToNonThrowing()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0
_ = ThrowingToNonThrowing.foo()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0

class ThrowingToNonThrowingReversed {
  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public init() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public init() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public static func foo() {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public static func foo() throws {} // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = ThrowingToNonThrowingReversed()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0
_ = ThrowingToNonThrowingReversed.foo()
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0

class ChangePropertyType {

  // We don't allow this for stored properties.

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public var stored: Int16 = 0

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public var stored: Int8 = 0 // CHECK: :[[@LINE]]:{{.+}} error: invalid redeclaration of 'stored'

  // OK for computed properties.

  @available(swift 5.0)
  @available(*, deprecated, message: "yes 5.0")
  public var computed: Int16 { get { } set { } }

  @available(swift, obsoleted: 5.0)
  @available(*, deprecated, message: "not 5.0")
  public var computed: Int8 { get { } set { } } // NEGATIVE-NOT: :[[@LINE]]:{{.+}}error
}

_ = ChangePropertyType().computed
// CHECK-4: :[[@LINE-1]]:{{.+}} not 5.0
// CHECK-5: :[[@LINE-2]]:{{.+}} yes 5.0
