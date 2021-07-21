// RUN: %target-typecheck-verify-swift %S/Inputs/sendable_cycle_other.swift -enable-experimental-concurrency -disable-availability-checking
// REQUIRES: concurrency

struct Bar {
  lazy var foo = {
    self.x()
  }

  func x() -> Int { 42 }
}
