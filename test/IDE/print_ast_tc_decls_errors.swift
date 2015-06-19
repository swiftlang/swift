// Verify errors in this file to ensure that parse and type checker errors
// occur where we expect them.
// RUN: %target-parse-verify-swift -show-diagnostics-after-fatal %s

// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -prefer-type-repr=false > %t.printed.txt
// RUN: FileCheck %s -strict-whitespace < %t.printed.txt
// RUN: FileCheck -check-prefix=NO-TYPEREPR %s -strict-whitespace < %t.printed.txt

// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename %s -prefer-type-repr=true > %t.printed.txt
// RUN: FileCheck %s -strict-whitespace < %t.printed.txt
// RUN: FileCheck -check-prefix=TYPEREPR %s -strict-whitespace < %t.printed.txt

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
// NO-TYPEREPR: {{^}}struct StructWithInheritance1 : <<error type>> {{{$}}
// TYPEREPR: {{^}}struct StructWithInheritance1 : FooNonExistentProtocol {{{$}}

struct StructWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// NO-TYPEREPR: {{^}}struct StructWithInheritance2 : <<error type>>, <<error type>> {{{$}}
// TYPEREPR: {{^}}struct StructWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {{{$}}

//===---
//===--- Inheritance list in classes.
//===---

class ClassWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}class ClassWithInheritance1 : <<error type>> {{{$}}
// TYREPR: {{^}}class ClassWithInheritance1 : FooNonExistentProtocol {{{$}}

class ClassWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// NO-TYREPR: {{^}}class ClassWithInheritance2 : <<error type>>, <<error type>> {{{$}}
// TYREPR: {{^}}class ClassWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {{{$}}

class ClassWithInheritance3 : FooClass, FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}class ClassWithInheritance3 : FooClass, <<error type>> {{{$}}
// TYREPR: {{^}}class ClassWithInheritance3 : FooClass, FooNonExistentProtocol {{{$}}

class ClassWithInheritance4 : FooProtocol, FooClass {} // expected-error {{superclass 'FooClass' must appear first in the inheritance clause}}
// CHECK: {{^}}class ClassWithInheritance4 : FooProtocol, FooClass {{{$}}

class ClassWithInheritance5 : FooProtocol, BarProtocol, FooClass {} // expected-error {{superclass 'FooClass' must appear first in the inheritance clause}}
// CHECK: {{^}}class ClassWithInheritance5 : FooProtocol, BarProtocol, FooClass {{{$}}

class ClassWithInheritance6 : FooClass, BarClass {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// NO-TYREPR: {{^}}class ClassWithInheritance6 : FooClass, <<error type>> {{{$}}
// TYREPR: {{^}}class ClassWithInheritance6 : FooClass, BarClass {{{$}}

class ClassWithInheritance7 : FooClass, BarClass, FooProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// NO-TYREPR: {{^}}class ClassWithInheritance7 : FooClass, <<error type>>, FooProtocol {{{$}}
// TYREPR: {{^}}class ClassWithInheritance7 : FooClass, BarClass, FooProtocol {{{$}}

class ClassWithInheritance8 : FooClass, BarClass, FooProtocol, BarProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}}
// NO-TYREPR: {{^}}class ClassWithInheritance8 : FooClass, <<error type>>, FooProtocol, BarProtocol {{{$}}
// TYREPR: {{^}}class ClassWithInheritance8 : FooClass, BarClass, FooProtocol, BarProtocol {{{$}}

class ClassWithInheritance9 : FooClass, BarClass, FooProtocol, BarProtocol, FooNonExistentProtocol {} // expected-error {{multiple inheritance from classes 'FooClass' and 'BarClass'}} expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}class ClassWithInheritance9 : FooClass, <<error type>>, FooProtocol, BarProtocol, <<error type>> {{{$}}
// TYREPR: {{^}}class ClassWithInheritance9 : FooClass, BarClass, FooProtocol, BarProtocol, FooNonExistentProtocol {{{$}}

//===---
//===--- Inheritance list in enums.
//===---

enum EnumWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// expected-error@-1{{type 'EnumWithInheritance1' does not conform to protocol 'RawRepresentable'}}
// NO-TYREPR: {{^}}enum EnumWithInheritance1 : <<error type>> {{{$}}
// TYREPR: {{^}}enum EnumWithInheritance1 : FooNonExistentProtocol {{{$}}

enum EnumWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// expected-error@-1{{type 'EnumWithInheritance2' does not conform to protocol 'RawRepresentable'}}
// NO-TYREPR: {{^}}enum EnumWithInheritance2 : <<error type>>, <<error type>> {{{$}}
// TYREPR: {{^}}enum EnumWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {{{$}}

enum EnumWithInheritance3 : FooClass { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}}
// expected-error@-1{{type 'EnumWithInheritance3' does not conform to protocol 'RawRepresentable'}}
// NO-TYREPR: {{^}}enum EnumWithInheritance3 : <<error type>> {{{$}}
// TYREPR: {{^}}enum EnumWithInheritance3 : FooClass {{{$}}

enum EnumWithInheritance4 : FooClass, FooProtocol { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}}
// expected-error@-1{{type 'EnumWithInheritance4' does not conform to protocol 'RawRepresentable'}}
// NO-TYREPR: {{^}}enum EnumWithInheritance4 : <<error type>>, FooProtocol {{{$}}
// TYREPR: {{^}}enum EnumWithInheritance4 : FooClass, FooProtocol {{{$}}

enum EnumWithInheritance5 : FooClass, BarClass { case X } // expected-error {{raw type 'FooClass' is not convertible from any literal}} expected-error {{multiple enum raw types 'FooClass' and 'BarClass'}}
// expected-error@-1{{type 'EnumWithInheritance5' does not conform to protocol 'RawRepresentable'}}
// NO-TYREPR: {{^}}enum EnumWithInheritance5 : <<error type>>, <<error type>> {{{$}}
// TYREPR: {{^}}enum EnumWithInheritance5 : FooClass, BarClass {{{$}}

//===---
//===--- Inheritance list in protocols.
//===---

protocol ProtocolWithInheritance1 : FooNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}protocol ProtocolWithInheritance1 : <<error type>> {{{$}}
// TYREPR: {{^}}protocol ProtocolWithInheritance1 : FooNonExistentProtocol {{{$}}

protocol ProtocolWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {} // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// NO-TYREPR: {{^}}protocol ProtocolWithInheritance2 : <<error type>>, <<error type>> {{{$}}
// TYREPR: {{^}}protocol ProtocolWithInheritance2 : FooNonExistentProtocol, BarNonExistentProtocol {{{$}}

protocol ProtocolWithInheritance3 : FooClass {} // expected-error {{non-class type 'ProtocolWithInheritance3' cannot inherit from class 'FooClass'}}
// NO-TYREPR: {{^}}protocol ProtocolWithInheritance3 : <<error type>> {{{$}}
// TYREPR: {{^}}protocol ProtocolWithInheritance3 : FooClass {{{$}}

protocol ProtocolWithInheritance4 : FooClass, FooProtocol {} // expected-error {{non-class type 'ProtocolWithInheritance4' cannot inherit from class 'FooClass'}}
// NO-TYREPR: {{^}}protocol ProtocolWithInheritance4 : <<error type>>, FooProtocol {{{$}}
// TYREPR: {{^}}protocol ProtocolWithInheritance4 : FooClass, FooProtocol {{{$}}

protocol ProtocolWithInheritance5 : FooClass, BarClass {} // expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'FooClass'}} expected-error {{non-class type 'ProtocolWithInheritance5' cannot inherit from class 'BarClass'}}
// NO-TYREPR: {{^}}protocol ProtocolWithInheritance5 : <<error type>>, <<error type>> {{{$}}
// TYREPR: {{^}}protocol ProtocolWithInheritance5 : FooClass, BarClass {{{$}}

//===---
//===--- Typealias printing.
//===---

// Normal typealiases.

typealias Typealias1 = FooNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}typealias Typealias1 = <<error type>>{{$}}
// TYREPR: {{^}}typealias Typealias1 = FooNonExistentProtocol{{$}}

// Associated types.

protocol AssociatedType1 {
// CHECK-LABEL: AssociatedType1 {

  typealias AssociatedTypeDecl1 : FooProtocol = FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl1 : FooProtocol = FooClass{{$}}

  typealias AssociatedTypeDecl2 : BazProtocol = FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl2 : BazProtocol = FooClass{{$}}

  typealias AssociatedTypeDecl3 : FooNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}}
// NO-TYREPR: {{^}}  typealias AssociatedTypeDecl3 : <<error type>>{{$}}
// TYREPR: {{^}}  typealias AssociatedTypeDecl3 : FooNonExistentProtocol{{$}}

  typealias AssociatedTypeDecl4 : FooNonExistentProtocol, BarNonExistentProtocol // expected-error {{use of undeclared type 'FooNonExistentProtocol'}} expected-error {{use of undeclared type 'BarNonExistentProtocol'}}
// NO-TYREPR: {{^}}  typealias AssociatedTypeDecl4 : <<error type>>, <<error type>>{{$}}
// TYREPR: {{^}}  typealias AssociatedTypeDecl4 : FooNonExistentProtocol, BarNonExistentProtocol{{$}}

  typealias AssociatedTypeDecl5 : FooClass
// CHECK: {{^}}  typealias AssociatedTypeDecl5 : FooClass{{$}}
}

//===---
//===--- Variable declaration printing.
//===---

var topLevelVar1 = 42
// CHECK: {{^}}var topLevelVar1: Int{{$}}
// CHECK-NOT: topLevelVar1

// CHECK: class C1
class C1 {
  // CHECK: init(data: )
  init(data:) // expected-error {{expected parameter type following ':'}}
}
