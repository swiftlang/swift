// RUN: %swift -parse %s -verify

func capture_nested_class() {
  var a = 5
  class Inner {
    func foo() -> Int {
      return a // FIXME: should probably be disallowed
    }
  }
}
