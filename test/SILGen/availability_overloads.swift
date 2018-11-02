// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/availability_overloads_other.swiftmodule -emit-module -primary-file %S/Inputs/availability_overloads_other.swift

// RUN: %target-swift-frontend -swift-version 3 -I %t -emit-silgen -primary-file %s
// RUN: %target-swift-frontend -swift-version 3 -I %t -emit-silgen -emit-ir %s

// RUN: %target-swift-frontend -swift-version 4 -I %t -emit-silgen -primary-file %s
// RUN: %target-swift-frontend -swift-version 4 -I %t -emit-silgen -emit-ir %s

// RUN: %target-swift-frontend -swift-version 3 -I %t -emit-module -emit-module-path /dev/null -primary-file %s
// RUN: %target-swift-frontend -swift-version 4 -I %t -emit-module -emit-module-path /dev/null -primary-file %s

// This is a "don't crash with duplicate definition errors" test.
// We care about being able to express each of these "redeclarations" when the
// availability doesn't overlap.

import availability_overloads_other

// FIXME: What about method overrides and protocol witnesses?

public class BeforeAndAfter {
  @available(swift, obsoleted: 4.0)
  public init(foo: ()) {}

  @available(swift 4.0)
  public init?(foo: ()) {}

  @available(swift, obsoleted: 4.0)
  public init() {}

  @available(swift 4.0)
  public init() throws {}

  @available(swift, obsoleted: 4.0)
  public static func foo() {}

  @available(swift 4.0)
  public static func foo() throws {}

  @available(swift 4.0)
  public var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 4.0)
  public var computed: Int8 { get { return 0 } set { } }

  @available(swift 4.0)
  public static var computed: Int16 { get { return 0 } set { } }

  @available(swift, obsoleted: 4.0)
  public static var computed: Int8 { get { return 0 } set { } }
}


// Make sure we can generate calls to these overloads, too
_ = BeforeAndAfter(foo: ())
_ = try BeforeAndAfter()
_ = try BeforeAndAfter.foo()
_ = BeforeAndAfter.computed
BeforeAndAfter.computed = 10
_ = try BeforeAndAfter().computed
try BeforeAndAfter().computed = 10

// Same thing but in a different module
_ = BeforeAndAfterOther(foo: ())
_ = try BeforeAndAfterOther()
_ = try BeforeAndAfterOther.foo()
_ = BeforeAndAfterOther.computed
BeforeAndAfterOther.computed = 10
_ = try BeforeAndAfterOther().computed
try BeforeAndAfterOther().computed = 10
