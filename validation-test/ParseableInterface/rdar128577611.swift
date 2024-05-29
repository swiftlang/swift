// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

struct InternalStruct {}
extension [Int: InternalStruct]: Sendable {}

// CHECK: @available(*, unavailable)
// CHECK: extension Swift.Dictionary : Swift.Copyable, Swift.Escapable, Swift.Sendable where Key : _ConstraintThatIsNotPartOfTheAPIOfThisLibrary {}
