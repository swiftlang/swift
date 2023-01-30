// RUN: not %target-swift-frontend %s -typecheck

struct Data {
  // Type `Q` not declared yet.
  typealias ReturnType = Q

  mutating func withContiguousMutableStorageIfAvailable<R>(
    _ body: ()  -> R
    // If you put "R" instead of "ReturnType", it doesn't crash.
  ) -> ReturnType {
    fatalError()
  }
}

func withUnsafeMutableBufferPointer<R>() -> R {
  var data = Data()
  return data.withContiguousMutableStorageIfAvailable {
    _ = Int.init
    return 2 as Any as R
  }
}
