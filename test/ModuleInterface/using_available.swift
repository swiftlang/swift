// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -module-name Legacy \
// RUN:     -enable-library-evolution \
// RUN:     -swift-version 5 \
// RUN:     -o %t/Legacy.swiftmodule \
// RUN:     -emit-module-interface-path %t/Legacy.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     %t/legacy.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/Legacy.swiftinterface) -module-name Legacy

// RUN: %FileCheck %t/legacy.swift --input-file %t/Legacy.swiftinterface

// RUN: %empty-directory(%t/top)
// RUN: %target-swift-frontend -emit-module -module-name Ordering \
// RUN:     -enable-library-evolution \
// RUN:     -swift-version 5 \
// RUN:     -o %t/top/Ordering.swiftmodule \
// RUN:     -emit-module-interface-path %t/top/Ordering.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     -DTOP %t/ordering.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/top/Ordering.swiftinterface) -module-name Ordering
// RUN: %FileCheck %t/ordering.swift --input-file %t/top/Ordering.swiftinterface

// RUN: %empty-directory(%t/bottom)
// RUN: %target-swift-frontend -emit-module -module-name Ordering \
// RUN:     -enable-library-evolution \
// RUN:     -swift-version 5 \
// RUN:     -o %t/bottom/Ordering.swiftmodule \
// RUN:     -emit-module-interface-path %t/bottom/Ordering.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     -DBOTTOM %t/ordering.swift
// RUN: %target-swift-typecheck-module-from-interface(%t/bottom/Ordering.swiftinterface) -module-name Ordering
// RUN: %FileCheck %t/ordering.swift --input-file %t/bottom/Ordering.swiftinterface

// RUN: diff %t/top/Ordering.swiftinterface %t/bottom/Ordering.swiftinterface

// make sure filecheck fails with neither flag
// RUN: %empty-directory(%t/neither)
// RUN: %target-swift-frontend -emit-module -module-name Ordering \
// RUN:     -enable-library-evolution \
// RUN:     -swift-version 5 \
// RUN:     -o %t/neither/Ordering.swiftmodule \
// RUN:     -emit-module-interface-path %t/neither/Ordering.swiftinterface \
// RUN:     -enable-experimental-feature DefaultIsolationPerFile \
// RUN:     %t/ordering.swift
// RUN: not %FileCheck %t/ordering.swift --input-file %t/neither/Ordering.swiftinterface

// RUN: %target-swift-frontend -typecheck -verify %t/consumer.swift -I %t -I %t/top
// RUN: %target-swift-frontend -typecheck -verify %t/consumer.swift -I %t -I %t/bottom

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
#if TOP
using @available(*, deprecated, message: "first default")
using @available(*, deprecated, message: "second default")
#endif

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

#if BOTTOM
using @available(*, deprecated, message: "first default")
using @available(*, deprecated, message: "second default")
#endif

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
