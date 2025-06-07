// RUN: %batch-code-completion

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

// Global completions
// WITH_GLOBAL_TYPES-DAG: Decl[Struct]/CurrModule:    FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Enum]/CurrModule:      FooEnum[#FooEnum#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Class]/CurrModule:     FooClass[#FooClass#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Protocol]/CurrModule:  FooProtocol[#FooProtocol#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[TypeAlias]/CurrModule: FooTypealias[#Int#]{{; name=.+$}}

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// WITHOUT_GLOBAL_TYPES-NOT: FooStruct
// WITHOUT_GLOBAL_TYPES-NOT: FooEnum
// WITHOUT_GLOBAL_TYPES-NOT: FooClass
// WITHOUT_GLOBAL_TYPES-NOT: FooProtocol
// WITHOUT_GLOBAL_TYPES-NOT: FooTypealias

// ERROR_COMMON: found code completion token

//===---
//===--- Test that we can complete types inside function parameters and
//===--- function result types.
//===---

func testTypeInParam1(a: #^TYPE_IN_FUNC_PARAM_1?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam1 {
  init(a: #^TYPE_IN_CONSTRUCTOR_PARAM_1?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam2(a: Int, b: #^TYPE_IN_FUNC_PARAM_2?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam2 {
  init(a: Int, b: #^TYPE_IN_CONSTRUCTOR_PARAM_2?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam3(a: unknown_type, b: #^TYPE_IN_FUNC_PARAM_3?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam3 {
  init(a: unknown_type, b: #^TYPE_IN_CONSTRUCTOR_PARAM_3?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam4(a: , b: #^TYPE_IN_FUNC_PARAM_4?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam4 {
  init(a: , b: #^TYPE_IN_CONSTRUCTOR_PARAM_4?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam5(a: b: #^TYPE_IN_FUNC_PARAM_5?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE;xfail=FIXME^#

struct TestTypeInConstructorParam5 {
  init(a: b: #^TYPE_IN_CONSTRUCTOR_PARAM_5?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE;xfail=FIXME^#
}


func testTypeInParam6(var a: #^TYPE_IN_FUNC_PARAM_6?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam6 {
  init(var a: #^TYPE_IN_CONSTRUCTOR_PARAM_6?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam7(let a: #^TYPE_IN_FUNC_PARAM_7?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam7 {
  init(let a: #^TYPE_IN_CONSTRUCTOR_PARAM_7?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam8(a: Int, var b: #^TYPE_IN_FUNC_PARAM_8?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam8 {
  init(a: Int, var a: #^TYPE_IN_CONSTRUCTOR_PARAM_8?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInParam9(a: Int, let b: #^TYPE_IN_FUNC_PARAM_9?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

struct TestTypeInConstructorParam9 {
  init(a: Int, let a: #^TYPE_IN_CONSTRUCTOR_PARAM_9?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
}


func testTypeInResult1() -> #^TYPE_IN_FUNC_RESULT_1?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult2(a) -> #^TYPE_IN_FUNC_RESULT_2?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult3(a:) -> #^TYPE_IN_FUNC_RESULT_3?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult4(a: Int) -> #^TYPE_IN_FUNC_RESULT_4?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult5(a: Int, ) -> #^TYPE_IN_FUNC_RESULT_5?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult6(a: Int, b ) -> #^TYPE_IN_FUNC_RESULT_6?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult7(a: Int, b: ) -> #^TYPE_IN_FUNC_RESULT_7?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
func testTypeInResult8(a: Int, b: unknown_type) -> #^TYPE_IN_FUNC_RESULT_8?check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#

// TYPE_IN_PARAM_FORWARD-DAG: Decl[TypeAlias]/CurrNominal: NestedTypealias[#Int#]{{; name=.+$}}

class TestTypesForwardInParam1 {
  init(a: #^TYPE_IN_PARAM_FORWARD_1?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam2 {
  init(a: #^TYPE_IN_PARAM_FORWARD_2?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam3 {
  init(withInt a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_3?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam4 {
  init(withInt a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_4?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam5 {
  func instanceFunc(a: #^TYPE_IN_PARAM_FORWARD_5?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam6 {
  func instanceFunc(a: #^TYPE_IN_PARAM_FORWARD_6?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam7 {
  func instanceFunc(a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_7?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#
  typealias NestedTypealias = Int
}

class TestTypesForwardInParam8 {
  func instanceFunc(a: Int, withFloat b: #^TYPE_IN_PARAM_FORWARD_8?check=TYPE_IN_PARAM_FORWARD;check=WITH_GLOBAL_TYPES;check=GLOBAL_NEGATIVE^#) {
    unknown_var = 42
  }
  typealias NestedTypealias = Int
}

class TestTypesWithoutName1 {
  func instanceFunc(#^TYPE_NO_NAME_1?check=NO_COMPLETION^#) {
  }
}

class TestTypesWithoutName2 {
  func instanceFunc(a : Int, #^TYPE_NO_NAME_2?check=NO_COMPLETION^#) {
  }
}

// NO_COMPLETION-NOT: Begin completions
