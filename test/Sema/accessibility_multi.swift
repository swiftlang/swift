// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/accessibility_multi_other.swift -verify
func read(_ value: Int) {}
func reset(_ value: inout Int) { value = 0 }

func testGlobals() {
  read(privateSetGlobal)
  privateSetGlobal = 42 // expected-error {{cannot assign to value: 'privateSetGlobal' setter is inaccessible}}
  reset(&privateSetGlobal) // expected-error {{cannot pass immutable value as inout argument: 'privateSetGlobal' setter is inaccessible}}
}

func testProperties(_ instance: Members) {
  var instance = instance
  read(instance.privateSetProp)
  instance.privateSetProp = 42 // expected-error {{cannot assign to property: 'privateSetProp' setter is inaccessible}}
  reset(&instance.privateSetProp) // expected-error {{cannot pass immutable value as inout argument: 'privateSetProp' setter is inaccessible}}
}

func testSubscript(_ instance: Members) {
  var instance = instance
  read(instance[])
  instance[] = 42 // expected-error {{cannot assign through subscript: subscript setter is inaccessible}}
  reset(&instance[]) // expected-error {{cannot pass immutable value as inout argument: subscript setter is inaccessible}}
}

func testPrivateConformance(_ instance: PrivateConformance) {
  instance.publicExtensionMember()
  // expected-error@-1 {{'publicExtensionMember' is inaccessible due to 'fileprivate' protection level}}

  instance.internalExtensionMember()
  // expected-error@-1 {{'internalExtensionMember' is inaccessible due to 'fileprivate' protection level}}
}
