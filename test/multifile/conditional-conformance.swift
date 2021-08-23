// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/catch22.swiftmodule %S/Inputs/conditional-conformance-catch22.swift -module-name catch22 -enable-testing
// RUN: %target-swift-frontend -typecheck %s -I %t

@testable import catch22

extension Catch22: Comparable where Value: Comparable {
  public static func <(lhs: Catch22, rhs: Catch22) -> Bool { true }
}
