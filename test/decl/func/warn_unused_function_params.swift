// RUN: %target-typecheck-verify-swift -warn-unused-function-params

// SR-2849
final class FooContainer {
  func foo1(x: Int) { 
    // expected-warning@-1:13 {{function parameter 'x' was never used; consider replacing it with '_'}} {{13-14=_}}
    // expected-note@-2:13 {{insert '_' to ignore 'x' instead}} {{14-14= _}}
    let array = [1, 2, 3]
    print(array[0])
  }

  func foo2(_: Int) { // No warning
    let array = [1, 2, 3]
    print(array[0])
  }

  func foo3(_ y: Int) { // // expected-warning@:15 {{function parameter 'y' was never used; consider removing it}} {{14-16=}}
    let array = [1, 2, 3]
    print(array[0])
  }

  func foo4(_ y: Int) { // No warning as 'y' is used in the body
    let array = [1, 2, 3]
    print(array[y])
  }

  func foo5(x y: Int) {
    // expected-warning@-1:15 {{function parameter 'y' was never used; consider replacing the whole parameter with '_'}} {{13-16=_}}
    // expected-note@-2:15 {{replace 'y' with '_' instead}} {{15-16=_}}
    let array = [1, 2, 3]
    print(array[0])
  }

  func foo6(x _: Int) { // No warning
    let array = [1, 2, 3]
    print(array[0])
  }

  func foo7(x y: Int) { // No warning as 'y' is used in the body
    let array = [1, 2, 3]
    print(array[y])
  }

  // FIXME: Handle this as well
  func foo8(x: Int = 0) {
    let array = [1, 2, 3]
    print(array[0])
  }
}
