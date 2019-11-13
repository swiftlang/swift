// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/protocol-conformance-let-other.swift

public struct S : P {
  public var x: Int
}
