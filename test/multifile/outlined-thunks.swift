// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -module-name outlined_thunks %S/Inputs/outlined-thunks-other.swift %s
// RUN: %target-build-swift -emit-library -module-name outlined_thunks -whole-module-optimization %S/Inputs/outlined-thunks-other.swift %s

// rdar://problem/39470607

protocol P { }

private struct MyPrivate<T: P> {
  private var otherHelper: OtherInternal<T>? = nil
  init(_: T) { }
}

extension P {
  func foo(data: Any) {
    _ = MyPrivate(data as! Self)
  }
}
