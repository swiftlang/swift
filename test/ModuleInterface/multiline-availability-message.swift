// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %target-swift-frontend -compile-module-from-interface %t/Test.swiftinterface -o %t/Test.swiftmodule
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules -emit-module-interface-path - %t/Test.swiftmodule -module-name Test | %FileCheck %s

// CHECK: @available(*, unavailable, message: "First line.\nAnother line!")
// CHECK: public func catastrophicFunction()
@available(*,
  unavailable,
  message: """
    First line.
    Another line!
    """)
public func catastrophicFunction() {}

// CHECK: @available(*, unavailable, message: "\n    First line.\n    Another line!")
// CHECK: public func catastrophicFunction2()
@available(*, unavailable, message: "\n    First line.\n    Another line!")
public func catastrophicFunction2() {}