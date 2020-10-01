// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module-path %t/ExternalProtocol.swiftmodule %S/Inputs/external-protocol.swift -module-name ExternalProtocol
// RUN: %target-swift-frontend -typecheck -I %t %s

import ExternalProtocol

extension AnExternalProtocol {
  typealias TypeAlias = Int

  func methodUsingAlias(_ alias: Self.TypeAlias) {}
}
