// RUN: %target-typecheck-verify-swift

enum MyError: Error {
case fail
case bad
}

func unsafeBuffers() throws(MyError) {
  var a = [1, 2, 3]
  try a.withUnsafeBufferPointer { (buffer) throws(MyError) in
    throw .bad
  }

  try a.withUnsafeMutableBufferPointer { (buffer) throws(MyError) in
    throw .bad
  }
}
