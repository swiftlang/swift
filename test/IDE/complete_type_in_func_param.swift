// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_5 > %t.types.txt
// FIXME: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// FIXME: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_7 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_8 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_9 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_5 > %t.types.txt
// FIXME: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// FIXME: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_7 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_8 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_CONSTRUCTOR_PARAM_9 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_CURRIED_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_CURRIED_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_CURRIED_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_7 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_8 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_9 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_10 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_11 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_12 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_RESULT_13 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_5 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_6 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_7 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_PARAM_FORWARD_8 > %t.types.txt
// RUN: FileCheck %s -check-prefix=TYPE_IN_PARAM_FORWARD < %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

//===--- Helper types that are used in this test

struct FooStruct {
}

var fooObject: FooStruct

func fooFunc() -> FooStruct {
  return fooObject
}

enum FooEnum {
}

class FooClass {
}

protocol FooProtocol {
  var fooInstanceVar: Int
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a: Int) -> Double
  subscript(i: Int) -> Double
}

protocol BarProtocol {
  var barInstanceVar: Int
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a: Int) -> Double
}

typealias FooTypealias = Int

// WITH_GLOBAL_TYPES: Begin completions
// Global completions
// WITH_GLOBAL_TYPES-DAG: Decl[Struct]/CurrModule:    FooStruct[#FooStruct#]{{$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Enum]/CurrModule:      FooEnum[#FooEnum#]{{$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Class]/CurrModule:     FooClass[#FooClass#]{{$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Protocol]/CurrModule:  FooProtocol[#FooProtocol#]{{$}}
// WITH_GLOBAL_TYPES-DAG: Decl[TypeAlias]/CurrModule: FooTypealias[#Int#]{{$}}
// WITH_GLOBAL_TYPES: End completions

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// WITHOUT_GLOBAL_TYPES-NOT: FooStruct
// WITHOUT_GLOBAL_TYPES-NOT: FooEnum
// WITHOUT_GLOBAL_TYPES-NOT: FooClass
// WITHOUT_GLOBAL_TYPES-NOT: FooProtocol
// WITHOUT_GLOBAL_TYPES-NOT: FooTypealias

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

//===---
//===--- Test that we can complete types inside function parameters and
//===--- function result types.
//===---

func testTypeInParam1(a: #^TYPE_IN_FUNC_PARAM_1^#

struct TestTypeInConstructorParam1 {
  init(a: #^TYPE_IN_CONSTRUCTOR_PARAM_1^#
}


func testTypeInParam2(a: Int, b: #^TYPE_IN_FUNC_PARAM_2^#

struct TestTypeInConstructorParam2 {
  init(a: Int, b: #^TYPE_IN_CONSTRUCTOR_PARAM_2^#
}


func testTypeInParam3(a: unknown_type, b: #^TYPE_IN_FUNC_PARAM_3^#

struct TestTypeInConstructorParam3 {
  init(a: unknown_type, b: #^TYPE_IN_CONSTRUCTOR_PARAM_3^#
}


func testTypeInParam4(a: , b: #^TYPE_IN_FUNC_PARAM_4^#

struct TestTypeInConstructorParam4 {
  init(a: , b: #^TYPE_IN_CONSTRUCTOR_PARAM_4^#
}


func testTypeInParam5(a: b: #^TYPE_IN_FUNC_PARAM_5^#

struct TestTypeInConstructorParam5 {
  init(a: b: #^TYPE_IN_CONSTRUCTOR_PARAM_5^#
}


func testTypeInParam6(var a: #^TYPE_IN_FUNC_PARAM_6^#

struct TestTypeInConstructorParam6 {
  init(var a: #^TYPE_IN_CONSTRUCTOR_PARAM_6^#
}


func testTypeInParam7(let a: #^TYPE_IN_FUNC_PARAM_7^#

struct TestTypeInConstructorParam7 {
  init(let a: #^TYPE_IN_CONSTRUCTOR_PARAM_7^#
}


func testTypeInParam8(a: Int, var b: #^TYPE_IN_FUNC_PARAM_8^#

struct TestTypeInConstructorParam8 {
  init(a: Int, var a: #^TYPE_IN_CONSTRUCTOR_PARAM_8^#
}


func testTypeInParam9(a: Int, let b: #^TYPE_IN_FUNC_PARAM_9^#

struct TestTypeInConstructorParam9 {
  init(a: Int, let a: #^TYPE_IN_CONSTRUCTOR_PARAM_9^#
}


func testTypeInParamCurried1(a: unknown_type)(b: #^TYPE_IN_FUNC_PARAM_CURRIED_1^#
func testTypeInParamCurried2(a: unknown_type)(var b: #^TYPE_IN_FUNC_PARAM_CURRIED_2^#
func testTypeInParamCurried3(a: unknown_type)(let b: #^TYPE_IN_FUNC_PARAM_CURRIED_3^#


func testTypeInResult1() -> #^TYPE_IN_FUNC_RESULT_1^#
func testTypeInResult2(a) -> #^TYPE_IN_FUNC_RESULT_2^#
func testTypeInResult3(a:) -> #^TYPE_IN_FUNC_RESULT_3^#
func testTypeInResult4(a: Int) -> #^TYPE_IN_FUNC_RESULT_4^#
func testTypeInResult5(a: Int, ) -> #^TYPE_IN_FUNC_RESULT_5^#
func testTypeInResult6(a: Int, b ) -> #^TYPE_IN_FUNC_RESULT_6^#
func testTypeInResult7(a: Int, b: ) -> #^TYPE_IN_FUNC_RESULT_7^#
func testTypeInResult8(a: Int, b: unknown_type) -> #^TYPE_IN_FUNC_RESULT_8^#
func testTypeInResult9(a: Int, b: unknown_type)() -> #^TYPE_IN_FUNC_RESULT_9^#
func testTypeInResult10(a: Int, b: unknown_type)(c) -> #^TYPE_IN_FUNC_RESULT_10^#
func testTypeInResult11(a: Int, b: unknown_type)(c:) -> #^TYPE_IN_FUNC_RESULT_11^#
func testTypeInResult12(a: Int, b: unknown_type)(c: Int) -> #^TYPE_IN_FUNC_RESULT_12^#
func testTypeInResult13(a: Int, b: unknown_type)(c: Int,) -> #^TYPE_IN_FUNC_RESULT_13^#

// TYPE_IN_PARAM_FORWARD: Begin completions
// TYPE_IN_PARAM_FORWARD-DAG: Decl[TypeAlias]/CurrNominal: NestedTypealias[#Int#]{{$}}
// TYPE_IN_PARAM_FORWARD: End completions

class TestTypesForwardInParam1 {
  init(a: #^TYPE_IN_PARAM_FORWARD_1^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam2 {
  init(a: #^TYPE_IN_PARAM_FORWARD_2^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam3 {
  init(withInt a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_3^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam4 {
  init(withInt a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_4^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam5 {
  func instanceFunc(a: #^TYPE_IN_PARAM_FORWARD_5^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam6 {
  func instanceFunc(a: #^TYPE_IN_PARAM_FORWARD_6^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam7 {
  func instanceFunc(a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_7^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam8 {
  func instanceFunc(a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_8^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

