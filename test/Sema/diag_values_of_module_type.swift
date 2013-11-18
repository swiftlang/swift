// RUN: %swift %s -parse -verify -I=%S/Inputs

import diag_values_of_module_type_foo

//===--- Allowed uses of module names.

var goodGlobal1: swift.Int
var goodGlobal2 = swift.Int(0)
var goodGlobal3 = diag_values_of_module_type_foo.SomeStruct(0)

class GoodClass1 : diag_values_of_module_type_foo.SomeClass {}

struct GoodStruct1 : diag_values_of_module_type_foo.SomeProtocol {
  typealias Foo = swift.Int
}

struct GoodStruct2 {
  var instanceVar1: swift.Int
  var instanceVar2 = swift.Int(0)
  var instanceVar2 = diag_values_of_module_type_foo.SomeStruct(0)

  static var staticVar1: swift.Int
  static var staticVar2 = swift.Int(0)
  static var staticVar2 = diag_values_of_module_type_foo.SomeStruct(0)
}

enum GoodEnum {
  case Foo1(swift.Int)
}

protocol GoodProtocol1 : diag_values_of_module_type_foo.SomeProtocol {
  typealias GoodTypealias1 : diag_values_of_module_type_foo.SomeProtocol
}

typealias GoodTypealias1 = swift.Int

func goodTest1() {
  var a1 = swift.Int(0)

  var b1 : diag_values_of_module_type_foo.SomeClass
  var b2 : diag_values_of_module_type_foo.SomeClass.NestedClass
  var b3 : diag_values_of_module_type_foo.SomeStruct
  var b4 : diag_values_of_module_type_foo.SomeEnum
  var b5 : diag_values_of_module_type_foo.SomeProtocol
  var b6 : diag_values_of_module_type_foo.SomeTypealias

  var c1 = diag_values_of_module_type_foo.SomeClass()
  var c2 = diag_values_of_module_type_foo.SomeClass.NestedClass()
  var c3 = diag_values_of_module_type_foo.SomeClass.staticFunc1
  var c4 = diag_values_of_module_type_foo.SomeClass.staticFunc1()
  var c5 = diag_values_of_module_type_foo.SomeClass.staticVar1
  var c6 = diag_values_of_module_type_foo.SomeStruct()
  var c7 = diag_values_of_module_type_foo.SomeEnum.Foo
  // Can not default-construct a protocol.
  // var c8 = diag_values_of_module_type_foo.SomeProtocol()
  var c9 = diag_values_of_module_type_foo.SomeTypealias()

  var d1 = diag_values_of_module_type_foo.someGlobal
  diag_values_of_module_type_foo.someGlobal = 42

  var e1 = diag_values_of_module_type_foo.someFunc
  diag_values_of_module_type_foo.someFunc()
}

func goodTest2a(a: swift.Int) {}
func goodTest2b(a: swift.Int) withInt(b: swift.Int) {}

func goodTest3() -> swift.Int {}

func goodTest4<T : diag_values_of_module_type_foo.SomeProtocol>() {}

// We don't support this requirement now, but if (when) we do, this should
// work.
func goodTest5<T : diag_values_of_module_type_foo.SomeProtocol
              where T.Foo == swift.Int>() {}
// expected-error@-1 {{second type 'swift.Int' in same-type requirement does not refer to a generic parameter or associated type}}

//===--- Disallowed uses of module names.

var badGlobal1 = swift // expected-error {{expected module member name after module name}}

class BadClass1 {
  // FIXME: should be only a single diagnostic.
  var instanceVar1 = swift // expected-error 2{{expected module member name after module name}}
  func instanceFunc1() {
    instanceVar1 = swift // expected-error {{expected module member name after module name}}
  }
}

func badTest1() {
  var x = swift // expected-error {{expected module member name after module name}}
  x = swift // expected-error {{expected module member name after module name}}
}
func badTest2() {
  var x = 0
  x = swift // expected-error {{expected module member name after module name}} expected-error {{'module<swift>' is not convertible to 'Int'}}
}
func badTest3() {
  var x = swift. // expected-error {{postfix '.' is reserved}} expected-error {{expected member name following '.'}}
}
func badTest4() {
  swift // expected-error {{expected module member name after module name}}
}
func badTest5() {
  swift. // expected-error {{postfix '.' is reserved}} expected-error {{expected member name following '.'}}
}
func badTest6() {
  // FIXME: should be only a single diagnostic.
  var a = { () -> Int in
            var x = swift // expected-error 2{{expected module member name after module name}}
            return 42 }()
  var b = { swift }() // expected-error {{expected module member name after module name}}
  var c = { { swift }() }() // expected-error {{expected module member name after module name}}
}

