// RUN: %target-swift-frontend %s -emit-ir -o -

func f() {
  enum NotAnError: Swift.Error {
    case nope(length: Int)
  }
}

