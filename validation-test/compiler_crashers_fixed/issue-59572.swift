// RUN: %target-swift-emit-ir %s

// https://github.com/apple/swift/issues/59572

func foo<T: RawRepresentable>(src: Any, target: inout T) where T.RawValue == UInt {
  if let x = src as? UInt, let x = T(rawValue: x) {
    target = x
  }
}
