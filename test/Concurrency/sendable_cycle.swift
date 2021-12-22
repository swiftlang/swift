// RUN: %target-typecheck-verify-swift %S/Inputs/sendable_cycle_other.swift  -disable-availability-checking

struct Bar {
  lazy var foo = {
    self.x()
  }

  func x() -> Int { 42 }
}
