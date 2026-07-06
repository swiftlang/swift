// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/availability_overloads_other.swiftmodule -emit-module -primary-file %S/Inputs/availability_overloads_other.swift

// RUN: %target-swift-emit-silgen -swift-version 5 -I %t -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT-5
// RUN: %target-swift-emit-ir -swift-version 5 -I %t %s

// RUN: %target-swift-emit-silgen -swift-version 6 -I %t -primary-file %s | %FileCheck %s --check-prefixes=CHECK,CHECK-SWIFT-6
// RUN: %target-swift-emit-ir -swift-version 6 -I %t %s

// RUN: %target-swift-frontend -swift-version 5 -I %t -emit-module -emit-module-path /dev/null -primary-file %s
// RUN: %target-swift-frontend -swift-version 6 -I %t -emit-module -emit-module-path /dev/null -primary-file %s

// This is a "don't crash with duplicate definition errors" test.
// We care about being able to express each of these "redeclarations" when the
// availability doesn't overlap.

import availability_overloads_other

// FIXME: What about method overrides and protocol witnesses?

public class BeforeAndAfter {
  @available(swift, obsoleted: 6.0)
  public init(foo: ()) {}

  @available(swift 6.0)
  public init?(foo: ()) {}

  @available(swift, obsoleted: 6.0)
  public init() {}

  @available(swift 6.0)
  public init() throws {}

  @available(swift, obsoleted: 6.0)
  public static func foo() {}

  @available(swift 6.0)
  public static func foo() throws {}

  @available(swift 6.0)
  public var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 6.0)
  public var computed: Int8 { get { return 0 } set { } }

  @available(swift 6.0)
  public static var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 6.0)
  public static var computed: Int8 { get { return 0 } set { } }
}

extension BeforeAndAfter {
  @available(swift, obsoleted: 6.0)
  public func bar() {}

  @available(swift 6.0)
  public func bar() throws {}
}

// Make sure we can generate calls to these overloads, too
// CHECK-LABEL: sil{{.*}} @$s22availability_overloads9testLocalyyAA14BeforeAndAfterCKF
public func testLocal(_ b: BeforeAndAfter) throws {
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooACyt_tcfC
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooACSgyt_tcfC
  _ = BeforeAndAfter(foo: ())
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterCACycfC
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterCACyKcfC
  _ = try BeforeAndAfter()
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooyyFZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooyyKFZ
  _ = try BeforeAndAfter.foo()
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds4Int8VvgZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds5Int16VvgZ
  _ = BeforeAndAfter.computed
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds4Int8VvsZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds5Int16VvsZ
  BeforeAndAfter.computed = 10
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfter.computed!getter : (BeforeAndAfter) -> () -> Int8
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfter.computed!getter : (BeforeAndAfter) -> () -> Int16
  _ = try BeforeAndAfter().computed
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfter.computed!setter : (BeforeAndAfter) -> (Int8) -> ()
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfter.computed!setter : (BeforeAndAfter) -> (Int16) -> ()
  try BeforeAndAfter().computed = 10
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3baryyF
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3baryyKF
  try b.bar()
}
// CHECK: } // end sil function '$s22availability_overloads9testLocalyyAA14BeforeAndAfterCKF'

// Overload selection in a function that is itself obsoleted in swift 6.0 should
// still be based on the current swift version.
// CHECK-LABEL: sil{{.*}} @$s22availability_overloads18testLocalObsoletedyyAA14BeforeAndAfterCKF
@available(swift, obsoleted: 6.0)
public func testLocalObsoleted(_ b: BeforeAndAfter) throws {
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooACyt_tcfC
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooACSgyt_tcfC
  _ = BeforeAndAfter(foo: ())
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterCACycfC
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterCACyKcfC
  _ = try BeforeAndAfter()
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooyyFZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3fooyyKFZ
  _ = try BeforeAndAfter.foo()
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds4Int8VvgZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds5Int16VvgZ
  _ = BeforeAndAfter.computed
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds4Int8VvsZ
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC8computeds5Int16VvsZ
  BeforeAndAfter.computed = 10
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfter.computed!getter : (BeforeAndAfter) -> () -> Int8
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfter.computed!getter : (BeforeAndAfter) -> () -> Int16
  _ = try BeforeAndAfter().computed
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfter.computed!setter : (BeforeAndAfter) -> (Int8) -> ()
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfter.computed!setter : (BeforeAndAfter) -> (Int16) -> ()
  try BeforeAndAfter().computed = 10
  // CHECK-SWIFT-5:  function_ref @$s22availability_overloads14BeforeAndAfterC3baryyF
  // CHECK-SWIFT-6:  function_ref @$s22availability_overloads14BeforeAndAfterC3baryyKF
  try b.bar()
}
// CHECK: } // end sil function '$s22availability_overloads18testLocalObsoletedyyAA14BeforeAndAfterCKF'

// Same thing but in a different module
// CHECK-LABEL: sil{{.*}} @$s22availability_overloads15testOtherModuleyy0a1_B6_other014BeforeAndAfterD0CKF
public func testOtherModule(_ b: BeforeAndAfterOther) throws {
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3fooACyt_tcfC
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3fooACSgyt_tcfC
  _ = BeforeAndAfterOther(foo: ())
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherCACycfC
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherCACyKcfC
  _ = try BeforeAndAfterOther()
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3fooyyFZ
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3fooyyKFZ
  _ = try BeforeAndAfterOther.foo()
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC8computeds4Int8VvgZ
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC8computeds5Int16VvgZ
  _ = BeforeAndAfterOther.computed
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC8computeds4Int8VvsZ
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC8computeds5Int16VvsZ
  BeforeAndAfterOther.computed = 10
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfterOther.computed!getter : (BeforeAndAfterOther) -> () -> Int8, $@convention(method) (@guaranteed BeforeAndAfterOther) -> Int8
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfterOther.computed!getter : (BeforeAndAfterOther) -> () -> Int16, $@convention(method) (@guaranteed BeforeAndAfterOther) -> Int16
  _ = try BeforeAndAfterOther().computed
  // CHECK-SWIFT-5:  class_method {{.*}}, #BeforeAndAfterOther.computed!setter : (BeforeAndAfterOther) -> (Int8) -> ()
  // CHECK-SWIFT-6:  class_method {{.*}}, #BeforeAndAfterOther.computed!setter : (BeforeAndAfterOther) -> (Int16) -> ()
  try BeforeAndAfterOther().computed = 10
  // CHECK-SWIFT-5:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3baryyF
  // CHECK-SWIFT-6:  function_ref @$s28availability_overloads_other19BeforeAndAfterOtherC3baryyKF
  try b.bar()
}
// CHECK: } // end sil function '$s22availability_overloads15testOtherModuleyy0a1_B6_other014BeforeAndAfterD0CKF'
