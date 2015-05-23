// RUN: %target-swift-frontend -parse -primary-file %s %S/Inputs/accessibility_multi_other.swift -verify
func read(value: Int) {}
func reset(inout value: Int) { value = 0 }

func testGlobals() {
  read(privateSetGlobal)
  privateSetGlobal = 42 // expected-error {{cannot assign to value: 'privateSetGlobal' setter is inaccessible}}
  reset(&privateSetGlobal) // expected-error {{cannot pass immutable value as inout argument: 'privateSetGlobal' setter is inaccessible}}
}

func testProperties(var instance: Members) {
  read(instance.privateSetProp)
  instance.privateSetProp = 42 // expected-error {{cannot assign to property: 'privateSetProp' setter is inaccessible}}
  reset(&instance.privateSetProp) // expected-error {{cannot pass immutable value as inout argument: 'privateSetProp' setter is inaccessible}}
}

func testSubscript(var instance: Members) {
  read(instance[])
  instance[] = 42 // expected-error {{cannot assign through subscript: subscript setter is inaccessible}}
  reset(&instance[]) // expected-error {{cannot pass immutable value as inout argument: subscript setter is inaccessible}}
}
