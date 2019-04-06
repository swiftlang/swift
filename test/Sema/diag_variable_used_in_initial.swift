// RUN: %target-typecheck-verify-swift

class A1 {
  func foo1() {}
  func foo2() {
    var foo1 = foo1() // expected-error {{variable used within its own initial value}}
  }
}

class A2 {
  var foo1 = 2
  func foo2() {
    // FIXME: "the var" doesn't sound right.
    var foo1 = foo1 // expected-error {{variable used within its own initial value}}
  }
}

class A3 {
  func foo2() {
    // FIXME: this should also add fixit.
    var foo1 = foo1() // expected-error {{variable used within its own initial value}}{{none}}
  }
  func foo1() {}
}

class A4 {
  func foo2() {
    var foo1 = foo1 // expected-error {{variable used within its own initial value}}{{none}}
  }
}

func localContext() {
  class A5 {
    func foo1() {}
    func foo2() {
      var foo1 = foo1() // expected-error {{variable used within its own initial value}}
    }

    class A6 {
      func foo1() {}
      func foo2() {
        var foo1 = foo1() // expected-error {{variable used within its own initial value}}
      }
    }

    extension E { // expected-error {{declaration is only valid at file scope}}
      // expected-error@-1{{use of undeclared type 'E'}}
      class A7 {
        func foo1() {}
        func foo2() {
          var foo1 = foo1() // expected-error {{variable used within its own initial value}}
        }
      }
    }
  }
}

// SR-10233
class SR_10233_C1 {
  func toStr(from bar: Int) -> String {
    return String(bar)
  }
  
  func foo() {
    let toStr = toStr(from: 1)
    print(toStr)
  }
}

struct SR_10233_S1 {
  func bar() {
    let string = string(at: 2)
    print(string)
  }
  
  private func string(at index: Int) -> String {
    return "Test"
  }
}

func SR_10233_F1(label: Int) -> Int { return label + 1 }
var SR_10233_F1 = SR_10233_F1(label: 42)

let type = type(of: SR_10233_F1)
