// RUN: %target-parse-verify-swift -I %S/Inputs -enable-source-import

import diag_values_of_module_type_foo

//===--- Allowed uses of module names.

var zero: Int8 = 0
var goodGlobal1: Swift.Int
var goodGlobal2 = Swift.Int(zero)
var goodGlobal3 = diag_values_of_module_type_foo.SomeStruct(a: 0)

class GoodClass1 : diag_values_of_module_type_foo.SomeClass {}

struct GoodStruct1 : diag_values_of_module_type_foo.SomeProtocol {
  typealias Foo = Swift.Int
}

struct GoodStruct2 {
  var instanceVar1: Swift.Int
  var instanceVar2 = Swift.Int(zero)
  var instanceVar3 = diag_values_of_module_type_foo.SomeStruct(a: 0)

  static var staticVar1: Swift.Int = 42
  static var staticVar2 = Swift.Int(zero)
  static var staticVar3 = diag_values_of_module_type_foo.SomeStruct(a: 0)
}

enum GoodEnum {
  case Foo1(Swift.Int)
}

protocol GoodProtocol1 : diag_values_of_module_type_foo.SomeProtocol {
  typealias GoodTypealias1 : diag_values_of_module_type_foo.SomeProtocol
}

typealias GoodTypealias1 = Swift.Int

func goodTest1() {
  var _ = Swift.Int(zero)

  var _ : diag_values_of_module_type_foo.SomeClass
  var _ : diag_values_of_module_type_foo.SomeClass.NestedClass
  var _ : diag_values_of_module_type_foo.SomeStruct
  var _ : diag_values_of_module_type_foo.SomeEnum
  var _ : diag_values_of_module_type_foo.SomeExistential
  var _ : diag_values_of_module_type_foo.SomeTypealias

  var _ = diag_values_of_module_type_foo.SomeClass()
  var _ = diag_values_of_module_type_foo.SomeClass.NestedClass()
  var _ = diag_values_of_module_type_foo.SomeClass.staticFunc1
  var _ = diag_values_of_module_type_foo.SomeClass.staticFunc1()
  var _ = diag_values_of_module_type_foo.SomeClass.staticVar1
  var _ = diag_values_of_module_type_foo.SomeStruct()
  var _ = diag_values_of_module_type_foo.SomeEnum.Foo
  // Can not default-construct a protocol.
  // var _ = diag_values_of_module_type_foo.SomeExistential()
  var _ = diag_values_of_module_type_foo.SomeTypealias()

  var _ = diag_values_of_module_type_foo.someGlobal
  diag_values_of_module_type_foo.someGlobal = 42

  var _ = diag_values_of_module_type_foo.someFunc
  diag_values_of_module_type_foo.someFunc()
}

func goodTest2a(a: Swift.Int) {}
func goodTest2b(a: Swift.Int, withInt b: Swift.Int) {}

func goodTest3() -> Swift.Int {}

func goodTest4<T : diag_values_of_module_type_foo.SomeProtocol>(_: T) {}

func goodTest5<T : diag_values_of_module_type_foo.SomeProtocol
               where T.Foo == Swift.Int>(_: T) {}

//===--- Disallowed uses of module names.

var badGlobal1 = Swift // expected-error {{expected module member name after module name}}

class BadClass1 {
  var instanceVar1 = Swift // expected-error {{expected module member name after module name}}
  func instanceFunc1() {
    instanceVar1 = Swift // expected-error {{expected module member name after module name}}
  }
}

func badTest1() {
  var x = Swift // expected-error {{expected module member name after module name}}
  x = Swift // expected-error {{expected module member name after module name}}
  _ = x
}
func badTest2() {
  var x = 0
  x = Swift // expected-error {{expected module member name after module name}} expected-error {{cannot assign a value of type 'module<Swift>' to a value of type 'Int'}}
  _ = x
}
func badTest3() {
  var x = Swift. // expected-error {{postfix '.' is reserved}} expected-error {{expected member name following '.'}}
}
func badTest4() {
  Swift // expected-error {{expected module member name after module name}}
}
func badTest5() {
  Swift. // expected-error {{postfix '.' is reserved}} expected-error {{expected member name following '.'}}
}
func badTest6() {
  // FIXME: should be only a single diagnostic.
  var _ = { () -> Int in
            var _ = Swift // expected-error 2{{expected module member name after module name}}
            return 42 }()
  var _ = { Swift }() // expected-error {{expected module member name after module name}}
  var _ = { { Swift }() }() // expected-error {{expected module member name after module name}}
}

