// Verify errors in this file to ensure that parse and type checker errors
// occur where we expect them.
// RUN: %swift -parse -verify -show-diagnostics-after-fatal %s

// RUN: %swift-ide-test -print-ast-typechecked -source-filename %s -prefer-type-repr=false > %t.printed.txt
// RUN: FileCheck %s -strict-whitespace < %t.printed.txt

// RUN: %swift-ide-test -print-ast-typechecked -source-filename %s -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -strict-whitespace < %t.printed.txt

//===---
//===--- Helper types.
//===---

class FooClass {}
class BarClass {}

protocol FooProtocol {}
protocol BarProtocol {}
protocol BazProtocol { func baz() }
protocol QuxProtocol {
  typealias Qux
}

class FooProtocolImpl : FooProtocol {}
class FooBarProtocolImpl : FooProtocol, BarProtocol {}
class BazProtocolImpl : BazProtocol { func baz() {} }

//===---
//===--- Import printing.
//===---

import not_existent_module_a // expected-error{{no such module 'not_existent_module_a'}}
// CHECK: {{^}}import not_existent_module_a{{$}}
import not_existent_module_a.submodule // expected-error{{no such module}}
// CHECK-NEXT: {{^}}import not_existent_module_a.submodule{{$}}

@exported import not_existent_module_b // expected-error{{no such module 'not_existent_module_b'}}
// CHECK-NEXT: {{^}}@exported import not_existent_module_b{{$}}
@exported import not_existent_module_b.submodule // expected-error{{no such module}}
// CHECK-NEXT: {{^}}@exported import not_existent_module_b.submodule{{$}}

import struct   not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import struct not_existent_module_c.foo{{$}}
import class    not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import class not_existent_module_c.foo{{$}}
import enum     not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import enum not_existent_module_c.foo{{$}}
import protocol not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import protocol not_existent_module_c.foo{{$}}
import var      not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import var not_existent_module_c.foo{{$}}
import func     not_existent_module_c.foo // expected-error{{no such module 'not_existent_module_c'}}
// CHECK-NEXT: {{^}}import func not_existent_module_c.foo{{$}}

//===---
//===--- Inheritance list in structs.
//===---

struct StructWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}struct StructWithInheritance1 : <<error type>> {{{$}}

struct StructWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}struct StructWithInheritance2 : <<error type>>, <<error type>> {{{$}}

//===---
//===--- Inheritance list in classes.
//===---

class ClassWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}class ClassWithInheritance1 : <<error type>> {{{$}}

class ClassWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}class ClassWithInheritance2 : <<error type>>, <<error type>> {{{$}}

class ClassWithInheritance3 : FooClass, FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}class ClassWithInheritance3 : FooClass, <<error type>> {{{$}}

class ClassWithInheritance4 : FooProtocol, FooClass {} // expected-error {{superclass 'FooClass' must appear first in the inheritance clause}}
// CHECK: {{^}}class ClassWithInheritance4 : FooProtocol, FooClass {{{$}}

class ClassWithInheritance5 : FooProtocol, BarProtocol, FooClass {} // expected-error {{superclass 'FooClass' must appear first in the inheritance clause}}
// CHECK: {{^}}class ClassWithInheritance5 : FooProtocol, BarProtocol, FooClass {{{$}}

class ClassWithInheritance6 : FooClass, BarClass {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// CHECK: {{^}}class ClassWithInheritance6 : FooClass, <<error type>> {{{$}}

class ClassWithInheritance7 : FooClass, BarClass, FooProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// CHECK: {{^}}class ClassWithInheritance7 : FooClass, <<error type>>, FooProtocol {{{$}}

class ClassWithInheritance8 : FooClass, BarClass, FooProtocol, BarProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// CHECK: {{^}}class ClassWithInheritance8 : FooClass, <<error type>>, FooProtocol, BarProtocol {{{$}}

class ClassWithInheritance9 : FooClass, BarClass, FooProtocol, BarProtocol, FooNonExistentProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}} expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}class ClassWithInheritance9 : FooClass, <<error type>>, FooProtocol, BarProtocol, <<error type>> {{{$}}

//===---
//===--- Inheritance list in enums.
//===---

enum EnumWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}enum EnumWithInheritance1 : <<error type>> {{{$}}

enum EnumWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}enum EnumWithInheritance2 : <<error type>>, <<error type>> {{{$}}

enum EnumWithInheritance3 : FooClass { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}} expected-error{{enum cases require explicit raw values when the raw type is not integer literal convertible}}
// CHECK: {{^}}enum EnumWithInheritance3 : <<error type>> {{{$}}

enum EnumWithInheritance4 : FooClass, FooProtocol { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}} expected-error{{enum cases require explicit raw values when the raw type is not integer literal convertible}}
// CHECK: {{^}}enum EnumWithInheritance4 : <<error type>>, FooProtocol {{{$}}

enum EnumWithInheritance5 : FooClass, BarClass { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}} expected-error {{multiple enum raw types 'FooClass' and 'BarClass'}} expected-error{{enum cases require explicit raw values when the raw type is not integer literal convertible}}
// CHECK: {{^}}enum EnumWithInheritance5 : <<error type>>, <<error type>> {{{$}}

//===---
//===--- Inheritance list in protocols.
//===---

protocol ProtocolWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}protocol ProtocolWithInheritance1 : <<error type>> {{{$}}

protocol ProtocolWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}protocol ProtocolWithInheritance2 : <<error type>>, <<error type>> {{{$}}

protocol ProtocolWithInheritance3 : FooClass {} // expected-error {{non-class type 'ProtocolWithInheritance3' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance3 : <<error type>> {{{$}}

protocol ProtocolWithInheritance4 : FooClass, FooProtocol {} // expected-error {{non-class type 'ProtocolWithInheritance4' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance4 : <<error type>>, FooProtocol {{{$}}

protocol ProtocolWithInheritance5 : FooClass, BarClass {} // expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'FooClass'}} expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'BarClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance5 : <<error type>>, <<error type>> {{{$}}

//===---
//===--- Typealias printing.
//===---

// Normal typealiases.

typealias Typealias1 = FooNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}typealias Typealias1 = <<error type>>{{$}}

// Associated types.

protocol AssociatedType1 {
// CHECK-LABEL: AssociatedType1 {

  typealias AssociatedTypeDecl1 : FooProtocol = FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl1 : FooProtocol = FooClass{{$}}

  typealias AssociatedTypeDecl2 : BazProtocol = FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl2 : BazProtocol = FooClass{{$}}

  typealias AssociatedTypeDecl3 : FooNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}  typealias AssociatedTypeDecl3 : <<error type>>{{$}}

  typealias AssociatedTypeDecl4 : FooNonExistentProtocol, BarNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}  typealias AssociatedTypeDecl4 : <<error type>>, <<error type>>{{$}}

  typealias AssociatedTypeDecl5 : FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl5 : FooClass{{$}}
}

//===---
//===--- Variable declaration printing.
//===---

var topLevelVar1 = 42
// CHECK: {{^}}var topLevelVar1{{$}}
// CHECK-NOT: topLevelVar1

// CHECK: class C1
class C1 {
  // CHECK: init(data: )
  init(data:) // expected-error {{expected parameter type following ':'}}
}
