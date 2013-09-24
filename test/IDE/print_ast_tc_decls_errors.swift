// Verify errors in this file to ensure that parse and type checker errors
// occur where we expect them.
// RUN: %swift -parse -verify %s

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
protocol BazProtocol { func baz() } // expected-note {{protocol requires function 'baz' with type '() -> ()'}}
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

import [exported] not_existent_module_b // expected-error{{no such module 'not_existent_module_b'}}
// CHECK-NEXT: {{^}}import [exported] not_existent_module_b{{$}}
import [exported] not_existent_module_b.submodule // expected-error{{no such module}}
// CHECK-NEXT: {{^}}import [exported] not_existent_module_b.submodule{{$}}

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
// CHECK: {{^}}class ClassWithInheritance6 : FooClass, BarClass {{{$}}

class ClassWithInheritance7 : FooClass, BarClass, FooProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// CHECK: {{^}}class ClassWithInheritance7 : FooClass, BarClass, FooProtocol {{{$}}

class ClassWithInheritance8 : FooClass, BarClass, FooProtocol, BarProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// CHECK: {{^}}class ClassWithInheritance8 : FooClass, BarClass, FooProtocol, BarProtocol {{{$}}

class ClassWithInheritance9 : FooClass, BarClass, FooProtocol, BarProtocol, FooNonExistentProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}} expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}class ClassWithInheritance9 : FooClass, BarClass, FooProtocol, BarProtocol, <<error type>> {{{$}}

//===---
//===--- Inheritance list in enums.
//===---

enum EnumWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}enum EnumWithInheritance1 : <<error type>> {{{$}}

enum EnumWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}enum EnumWithInheritance2 : <<error type>>, <<error type>> {{{$}}

enum EnumWithInheritance3 : FooClass {} // expected-error {{raw type 'FooClass' is not integer-, float-, char-, or string-literal-convertible}}
// CHECK: {{^}}enum EnumWithInheritance3 : FooClass {{{$}}

enum EnumWithInheritance4 : FooClass, FooProtocol {} // expected-error {{raw type 'FooClass' is not integer-, float-, char-, or string-literal-convertible}}
// CHECK: {{^}}enum EnumWithInheritance4 : FooClass, FooProtocol {{{$}}

enum EnumWithInheritance5 : FooClass, BarClass {} // expected-error {{raw type 'FooClass' is not integer-, float-, char-, or string-literal-convertible}} expected-error {{multiple enum raw types 'FooClass' and 'BarClass'}}
// CHECK: {{^}}enum EnumWithInheritance5 : FooClass, BarClass {{{$}}

//===---
//===--- Inheritance list in protocols.
//===---

protocol ProtocolWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}protocol ProtocolWithInheritance1 : <<error type>> {{{$}}

protocol ProtocolWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}protocol ProtocolWithInheritance2 : <<error type>>, <<error type>> {{{$}}

protocol ProtocolWithInheritance3 : FooClass {} // expected-error {{non-class type 'ProtocolWithInheritance3' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance3 : FooClass {{{$}}

protocol ProtocolWithInheritance4 : FooClass, FooProtocol {} // expected-error {{non-class type 'ProtocolWithInheritance4' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance4 : FooClass, FooProtocol {{{$}}

protocol ProtocolWithInheritance5 : FooClass, BarClass {} // expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'FooClass'}} expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'BarClass'}}
// CHECK: {{^}}protocol ProtocolWithInheritance5 : FooClass, BarClass {{{$}}

//===---
//===--- Typealias printing.
//===---

// Normal typealiases.

typealias TypealiasWithInheritance1 : BazProtocol = FooClass // expected-error {{type 'TypealiasWithInheritance1' does not conform to protocol 'BazProtocol'}}
// CHECK: {{^}}typealias TypealiasWithInheritance1 : BazProtocol = FooClass{{$}}

typealias TypealiasWithInheritance2 : FooNonExistentProtocol = FooProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}typealias TypealiasWithInheritance2 : <<error type>> = FooProtocol{{$}}

typealias TypealiasWithInheritance3 : FooNonExistentProtocol, BarNonExistentProtocol = FooProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}typealias TypealiasWithInheritance3 : <<error type>>, <<error type>> = FooProtocol{{$}}

typealias TypealiasWithInheritance4 : FooClass = FooProtocol // expected-error {{non-class type 'TypealiasWithInheritance4' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}typealias TypealiasWithInheritance4 : FooClass = FooProtocol{{$}}

// Associated types.

protocol AssociatedType1 {
// CHECK-LABEL: AssociatedType1 {

  typealias AssociatedTypeDecl1 : FooProtocol = FooClass // expected-error {{typealias 'AssociatedTypeDecl1' in protocol cannot have a definition}}
// CHECK: {{^}}  typealias AssociatedTypeDecl1 : FooProtocol{{$}}

  typealias AssociatedTypeDecl2 : BazProtocol = FooClass // expected-error {{typealias 'AssociatedTypeDecl2' in protocol cannot have a definition}}
// CHECK: {{^}}  typealias AssociatedTypeDecl2 : BazProtocol{{$}}

  typealias AssociatedTypeDecl3 : FooNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// CHECK: {{^}}  typealias AssociatedTypeDecl3 : <<error type>>{{$}}

  typealias AssociatedTypeDecl4 : FooNonExistentProtocol, BarNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// CHECK: {{^}}  typealias AssociatedTypeDecl4 : <<error type>>, <<error type>>{{$}}

  typealias AssociatedTypeDecl5 : FooClass // expected-error {{non-class type 'AssociatedTypeDecl5' cannot inherit from class 'FooClass'}}
// CHECK: {{^}}  typealias AssociatedTypeDecl5 : FooClass{{$}}
}

