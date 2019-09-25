// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/sr11458.swift

// SR-11458: crash involving subscript in other file
class MyOtherType<T: Equatable>: Observed {
  @Observable var x: T

  init(x: T) {
    self.x = x
  }
}
