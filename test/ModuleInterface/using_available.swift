// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name Legacy \
// RUN:     -enable-library-evolution \
// RUN:     -o %t/Legacy.swiftmodule \
// RUN:     -emit-module-interface-path %t/Legacy.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     %t/legacy.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Legacy.swiftinterface) -module-name Legacy

// RUN: %FileCheck %t/legacy.swift --input-file %t/Legacy.swiftinterface

// RUN: %target-swift-frontend -emit-module -module-name Ordering \
// RUN:     -enable-library-evolution \
// RUN:     -o %t/Ordering.swiftmodule \
// RUN:     -emit-module-interface-path %t/Ordering.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     %t/ordering.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Ordering.swiftinterface) -module-name Ordering
// RUN: %FileCheck %t/ordering.swift --input-file %t/Ordering.swiftinterface

// RUN: %target-swift-frontend -typecheck -verify %t/consumer.swift -I %t

// REQUIRES: swift_feature_DefaultIsolationPerFile

//--- legacy.swift
// The `using` declaration itself must not appear in the emitted interface.
// CHECK-NOT: using @available

using @available(*, deprecated, message: "legacy module")

// CHECK:      @available(*, deprecated, message: "legacy module")
// CHECK-NEXT: public func defaultedFunc()
public func defaultedFunc() {}

// CHECK:      @available(*, deprecated, message: "legacy module")
// CHECK-NEXT: public class DefaultedClass
public class DefaultedClass {
  // CHECK-NOT: @available
  // CHECK:     public init()
  public init() {}
  // CHECK-NOT: @available
  // CHECK:     public func method()
  public func method() {}
}

// CHECK:      @available(*, deprecated, message: "legacy module")
// CHECK-NEXT: extension Legacy::DefaultedClass
extension DefaultedClass {
  public func extensionMethod() {}
}

// CHECK:      @available(*, deprecated, message: "legacy module")
// CHECK-NEXT: @available(*, deprecated, message: "explicit")
// CHECK-NEXT: public func explicitlyDeprecated()
@available(*, deprecated, message: "explicit")
public func explicitlyDeprecated() {}

//--- ordering.swift
// CHECK-NOT: using @available

// We want multiple available attrs to show up in the right order.
using @available(*, deprecated, message: "first default")
using @available(*, deprecated, message: "second default")

// CHECK:      @available(*, deprecated, message: "first default")
// CHECK-NEXT: @available(*, deprecated, message: "second default")
// CHECK-NEXT: public func defaultedOrderedFunc()
public func defaultedOrderedFunc() {}

// CHECK:      @available(*, deprecated, message: "first default")
// CHECK-NEXT: @available(*, deprecated, message: "second default")
// CHECK-NEXT: @available(*, deprecated, message: "explicit")
// CHECK-NEXT: public func orderedFunc()
@available(*, deprecated, message: "explicit")
public func orderedFunc() {}

//--- consumer.swift
import Legacy
import Ordering

func use() {
  defaultedFunc() // expected-warning {{'defaultedFunc()' is deprecated: legacy module}}
  _ = DefaultedClass() // expected-warning {{'DefaultedClass' is deprecated: legacy module}}
  explicitlyDeprecated() // expected-warning {{'explicitlyDeprecated()' is deprecated: explicit}}
}

func useOrdering() {
  // Last-listed default wins.
  defaultedOrderedFunc() // expected-warning {{'defaultedOrderedFunc()' is deprecated: second default}}
  orderedFunc() // expected-warning {{'orderedFunc()' is deprecated: explicit}}
}
