// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift

// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I=%S/Inputs -sdk "" -enable-access-control -verify
// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I=%S/Inputs -sdk "" -disable-access-control -D DEFINE_VAR_FOR_SCOPED_IMPORT

// RUN: %swift -emit-module -o %t %S/Inputs/has_accessibility.swift -D DEFINE_VAR_FOR_SCOPED_IMPORT
// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I=%t -sdk "" -enable-access-control -verify
// RUN: %swift -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I=%t -sdk "" -disable-access-control

import has_accessibility

// This deliberately has the wrong import kind.
import var has_accessibility.zz // expected-error {{no such decl in module}}

println(has_accessibility.x)
println(has_accessibility.y) // expected-error {{module 'has_accessibility' has no member named 'y'}}
println(has_accessibility.z) // expected-error {{module 'has_accessibility' has no member named 'z'}}

println(accessibility.a)
println(accessibility.b)
println(accessibility.c) // expected-error {{module 'accessibility' has no member named 'c'}}

println(x)
println(y) // expected-error {{use of unresolved identifier 'y'}}
println(z) // expected-error {{use of unresolved identifier 'z'}}
println(a)
println(b)
println(c) // expected-error {{use of unresolved identifier 'c'}}

Foo.x()
Foo.y() // expected-error {{'Foo.Type' does not have a member named 'y'}}
Foo.z() // expected-error {{'Foo.Type' does not have a member named 'z'}}
Foo.a()
Foo.b()
Foo.c() // expected-error {{'Foo.Type' does not have a member named 'c'}}

Foo() // expected-error {{'Foo' cannot be constructed because it has no accessible initializers}}
PrivateInit() // expected-error {{'PrivateInit' cannot be constructed because it has no accessible initializers}}

class Sub : Base {
  func test() {
    value = 4 // expected-error {{cannot assign to 'value' in 'self'}}
    self.value = 4 // expected-error {{cannot assign to 'value' in 'self'}}
    super.value = 4 // expected-error {{cannot assign to the result of this expression}}

    method() // expected-error {{use of unresolved identifier 'method'}}
    self.method() // expected-error {{'Sub' does not have a member named 'method'}}
    super.method() // expected-error {{'Base' does not have a member named 'method'}}
  }
}

class ObservingOverrider : Base { // expected-error {{class 'ObservingOverrider' has no initializers}}
  override var value: Int { // expected-error {{cannot observe read-only property 'value'; it can't change}} expected-note {{stored property 'value' without initial value prevents synthesized initializers}}
    willSet { println(newValue) }
  }
}

class ReplacingOverrider : Base {
  override var value: Int {
    get { return super.value }
    set { super.value = newValue } // expected-error {{cannot assign to the result of this expression}}
  }
}
