// RUN: %target-swift-frontend -module-name=test -emit-sil %s -enable-builtin-module

import Builtin

// Ensure compiler does not crash
struct Wrapper {
  var a = [1, 2, 3]
  init() {
    _withUnsafeMutablePointer(to: &a) {
      bar($0)
    }
  }
}

func _withUnsafeMutablePointer<T, E: Error, Result>(
  to value: inout T,
  _ body: (UnsafeMutablePointer<T>) throws(E) -> Result
) throws(E) -> Result {
  try body(UnsafeMutablePointer<T>(Builtin.addressof(&value)))
}

func bar<T>(_ arg: UnsafeMutablePointer<[T]>) {}

