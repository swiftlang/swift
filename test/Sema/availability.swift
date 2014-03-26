// RUN: %swift %s -verify

@availability(*, unavailable)
func unavailable_foo() {} // expected-note {{'unavailable_foo' has been explicitly marked unavailable here}}

func test() {
  unavailable_foo() // expected-error {{'unavailable_foo' is unavailable}}
}

