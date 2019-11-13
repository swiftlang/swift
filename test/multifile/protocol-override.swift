// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/other.swiftmodule %S/Inputs/protocol-override-other.swift -module-name other -enable-testing
// RUN: %target-swift-frontend -typecheck %s -I %t

@testable import other

protocol Sub : Base {
  var foo: String { get set }
}
