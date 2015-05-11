// RUN: %target-parse-verify-swift

func capture_nested_class() {
  var a = 5
  class Inner {
    func foo() -> Int {
      return a // FIXME: should probably be disallowed
    }
  }
  a = 1
  _ = a
}
