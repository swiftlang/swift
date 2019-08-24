// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-source-import

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
  associatedtype GoodTypealias1 : diag_values_of_module_type_foo.SomeProtocol
}

typealias GoodTypealias1 = Swift.Int

func goodTest1() {
  _ = Swift.Int(zero)

  var _ : diag_values_of_module_type_foo.SomeClass
  var _ : diag_values_of_module_type_foo.SomeClass.NestedClass
  var _ : diag_values_of_module_type_foo.SomeStruct
  var _ : diag_values_of_module_type_foo.SomeEnum
  var _ : diag_values_of_module_type_foo.SomeExistential
  var _ : diag_values_of_module_type_foo.SomeTypealias

  _ = diag_values_of_module_type_foo.SomeClass()
  _ = diag_values_of_module_type_foo.SomeClass.NestedClass()
  _ = diag_values_of_module_type_foo.SomeClass.staticFunc1
  _ = diag_values_of_module_type_foo.SomeClass.staticFunc1()
  _ = diag_values_of_module_type_foo.SomeClass.staticVar1
  _ = diag_values_of_module_type_foo.SomeStruct()
  _ = diag_values_of_module_type_foo.SomeEnum.Foo
  // Cannot default-construct a protocol.
  // _ = diag_values_of_module_type_foo.SomeExistential()
  _ = diag_values_of_module_type_foo.SomeTypealias()

  _ = diag_values_of_module_type_foo.someGlobal
  diag_values_of_module_type_foo.someGlobal = 42

  _ = diag_values_of_module_type_foo.someFunc
  diag_values_of_module_type_foo.someFunc()
}

func goodTest2a(a: Swift.Int) {}
func goodTest2b(a: Swift.Int, withInt b: Swift.Int) {}

func goodTest3() -> Swift.Int {}

func goodTest4<T : diag_values_of_module_type_foo.SomeProtocol>(_: T) {}

func goodTest5<T : diag_values_of_module_type_foo.SomeProtocol>(_: T)
  where T.Foo == Swift.Int {}

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
  x = Swift //  expected-error {{cannot assign value of type 'module<Swift>' to type 'Int'}}
  _ = x
}
func badTest3() {
  var _ = Swift. // expected-error {{expected member name following '.'}}
}
func badTest4() {
  _ = Swift // expected-error {{expected module member name after module name}}
}
func badTest5() {
  _ = Swift. // expected-error {{expected member name following '.'}}
}
func badTest6() {
  _ = { () -> Int in
            _ = Swift // expected-error {{expected module member name after module name}}
            return 42 }()
  _ = { Swift }() // expected-error {{expected module member name after module name}}
  _ = { { Swift }() }() // expected-error {{expected module member name after module name}}
}

