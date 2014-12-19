// RUN: %swift -parse %s -verify

// Simple case.
@autoclosure var fn : () -> Int = 4

@autoclosure func func1() {}  // expected-error {{'autoclosure' attribute cannot be applied to this declaration}}
@autoclosure var v1 : Int = 4 // expected-error {{'autoclosure' attribute may only be applied to values of function type}}


func func2(@autoclosure fp : () -> Int) { func2(4)}

func func3(@autoclosure fp fpx : () -> Int) {func3(fp: 0)}
func func4(@autoclosure #fp : () -> Int) {func4(fp: 0)}
func func5(@autoclosure var #fp : () -> Int) {func5(fp: 0)}
func func6(@autoclosure () -> Int) {func6(0)}

// declattr and typeattr on the argument.
func func7(@autoclosure @noreturn () -> Int) {func7(0)}


struct SomeStruct {
  @autoclosure let property : () -> Int  // autoclosures work as an property as well.

  init() {
    property = 4
    let a : Int = property()
  }
}

class BaseClass {
  // FIXME: rdar://19311652 - class properties don't work due to synthesized code issues
  @autoclosure var property : () -> Int = 4 // autoclosures work as an property as well.

}

class DerivedClass {
  var property : () -> Int { get {} set {} }
}


