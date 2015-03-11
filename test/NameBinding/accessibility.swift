// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift

// RUN: %target-swift-frontend -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I %S/Inputs -sdk "" -enable-access-control -verify
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -enable-source-import -I %S/Inputs -sdk "" -disable-access-control -D DEFINE_VAR_FOR_SCOPED_IMPORT -D ACCESS_DISABLED

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_accessibility.swift -D DEFINE_VAR_FOR_SCOPED_IMPORT
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I %t -sdk "" -enable-access-control -verify
// RUN: %target-swift-frontend -parse -primary-file %t/main.swift %S/Inputs/accessibility_other.swift -module-name accessibility -I %t -sdk "" -disable-access-control -D ACCESS_DISABLED

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

var s = StructWithPrivateSetter()
s.x = 42 // expected-error {{cannot assign to 'x' in 's'}}

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

class ObservingOverrider : Base {
  override var value: Int { // expected-error {{cannot observe read-only property 'value'; it can't change}}
    willSet { println(newValue) }
  }
}

class ReplacingOverrider : Base {
  override var value: Int {
    get { return super.value }
    set { super.value = newValue } // expected-error {{cannot assign to the result of this expression}}
  }
}


protocol MethodProto {
  func method() // expected-note + {{protocol requires function 'method()' with type '() -> ()'}}
}

extension OriginallyEmpty : MethodProto {}
extension HiddenMethod : MethodProto {} // expected-error {{type 'HiddenMethod' does not conform to protocol 'MethodProto'}}
#if !ACCESS_DISABLED
extension Foo : MethodProto {} // expected-error {{type 'Foo' does not conform to protocol 'MethodProto'}}
#endif


protocol TypeProto {
  typealias TheType // expected-note + {{protocol requires nested type 'TheType'}}
}

extension OriginallyEmpty : MethodProto {}
extension HiddenType : TypeProto {} // expected-error {{type 'HiddenType' does not conform to protocol 'TypeProto'}}
#if !ACCESS_DISABLED
extension Foo : TypeProto {} // expected-error {{type 'Foo' does not conform to protocol 'TypeProto'}}
#endif


#if !ACCESS_DISABLED
private func privateInBothFiles() {} // no-warning
private func privateInPrimaryFile() {} // expected-error {{invalid redeclaration}}
func privateInOtherFile() {} // expected-note {{previously declared here}}
#endif

