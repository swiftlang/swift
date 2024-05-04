// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -package-name Library -module-name Library -plugin-path %swift-plugin-dir -disable-availability-checking
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -package-name Library -module-name Library -disable-availability-checking
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: swift_swift_parser
// REQUIRES: observation

import Observation

// CHECK-NOT: @Observable
// CHECK-NOT: @ObservationIgnored
// CHECK-NOT: @ObservationTracked

@Observable
public class SomeClass {
  public var field = 3
  package var test = 4
  @ObservationIgnored public var ignored = 4
}

public func requiresObservable<T: Observable>(_: T) { }

@inlinable func useObservable(sc: SomeClass) {
  requiresObservable(sc)
}
