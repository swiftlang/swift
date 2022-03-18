// RUN: %target-typecheck-verify-swift

func takeFn(@_implicitSelfCapture fn: @escaping () -> Int) { }
func takeEscapingFn(fn: @escaping () -> Int) { }

class C {
  var property: Int = 0

  func method() { }

  func testMethod() {
    takeFn { // no errors
      method()
      return property
    }

    takeEscapingFn { // expected-note 2 {{capture 'self' explicitly to enable implicit 'self' in this closure}}
        // expected-note@-1{{explicitly capture 'property' to capture value in this closure}}{{21-21= [property] in}}
        // expected-note@-2{{explicitly capture 'method' to call method in this closure}}{{21-21= [method] in}}

      method() // expected-error{{call to method 'method' in closure requires explicit use of 'self' to make capture semantics explicit}}
      // expected-note@-1{{reference 'self.' explicitly}}
      return property // expected-error{{reference to property 'property' in closure requires explicit use of 'self' to make capture semantics explicit}}
      // expected-note@-1{{reference 'self.' explicitly}}
    }
  }
}
