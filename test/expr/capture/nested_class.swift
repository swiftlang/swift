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

// <rdar://problem/18734297> Reject access to local variables from local types
func FunctionWithInnerStruct() -> Int {
  let b = 0  // expected-note {{'b' declared here}}
  struct c {  // expected-note {{type declared here}}
    var a = b  // expected-error {{struct declaration cannot close over value 'b' defined in outer scope}}
    
    init() {}
  }
  
  func f() { _ = b }
}

struct StructWithInnerStruct {
  let ivar = 42
  func f() { _ = ivar }
  
  func a() -> Int {  // expected-note 2 {{'self' declared here}}
    struct c {      // expected-note 2 {{type declared here}}
      var x = ivar  // expected-error {{struct declaration cannot close over value 'self' defined in outer scope}}
      var y = self.ivar // expected-error {{struct declaration cannot close over value 'self' defined in outer scope}}
      
      init() {}
    }
  }
}
