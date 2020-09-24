// Check that ASTScope lookup works for a construction found in GRDB's Row.swift
// RUN: %target-swift-frontend -typecheck %s

protocol P1 {}
protocol P2 {}

struct S<Value> {
  // Next line is the problematic one, finding P2
  subscript<Value: P1 & P2>(_ index: Int) -> Value? {
    return nil
  }
}
