// RUN: %target-swift-frontend -emit-ir %s

public func values<each T>() -> (repeat (each T)?) {
  (repeat { () -> (each T)? in
    nil
  }())
}
