// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/sr11458.swift

// https://github.com/apple/swift/issues/53859
// Crash involving subscript in other file

class MyOtherType<T: Equatable>: Observed {
  @Observable var x: T

  init(x: T) {
    self.x = x
  }
}
