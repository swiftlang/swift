// RUN: %target-parse-verify-swift

func capture_nested_class() {
  var a = 5 //expected-note{{'a' declared here}}
  class Inner { //expected-note{{type declared here}}
    func foo() -> Int {
      return a // expected-error{{class declaration cannot close over value 'a' defined in outer scope}}
    }
  }
  a = 1
  _ = a
}
