// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_1 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_2 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_3 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_4 | FileCheck %s -check-prefix=COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_IGNORED_1 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_IGNORED_2 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_IGNORED_3 | FileCheck %s -check-prefix=COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_1 > %t.param.txt
// RUN: FileCheck %s -check-prefix=COMMON < %t.param.txt
// RUN: FileCheck %s -check-prefix=FIND_FUNC_PARAM_1 < %t.param.txt
// RUN: FileCheck %s -check-prefix=NO_SELF < %t.param.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_2 > %t.param.txt
// RUN: FileCheck %s -check-prefix=COMMON < %t.param.txt
// RUN: FileCheck %s -check-prefix=FIND_FUNC_PARAM_2 < %t.param.txt
// RUN: FileCheck %s -check-prefix=NO_SELF < %t.param.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_3 | FileCheck %s -check-prefix=FIND_FUNC_PARAM_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_4 | FileCheck %s -check-prefix=FIND_FUNC_PARAM_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_5 | FileCheck %s -check-prefix=FIND_FUNC_PARAM_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_6 | FileCheck %s -check-prefix=FIND_FUNC_PARAM_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_7 | FileCheck %s -check-prefix=FIND_FUNC_PARAM_7

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_FUNC_PARAM_SELECTOR_1 > %t.param.txt
// RUN: FileCheck %s -check-prefix=COMMON < %t.param.txt
// RUN: FileCheck %s -check-prefix=FIND_FUNC_PARAM_SELECTOR_1 < %t.param.txt
// RUN: FileCheck %s -check-prefix=NO_SELF < %t.param.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_1 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_2 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_3 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_4 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_5 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_CONSTRUCTOR_PARAM_SELECTOR_1 | FileCheck %s -check-prefix=FIND_CONSTRUCTOR_PARAM_SELECTOR_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_DESTRUCTOR_PARAM_1 > %t.param.txt
// RUN: FileCheck %s -check-prefix=FIND_DESTRUCTOR_PARAM_1 < %t.param.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FIND_DESTRUCTOR_PARAM_2 > %t.param.txt
// RUN: FileCheck %s -check-prefix=FIND_DESTRUCTOR_PARAM_2 < %t.param.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_1 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_2 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_3 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_5 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_6 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_7 | FileCheck %s -check-prefix=COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INVALID_8 | FileCheck %s -check-prefix=COMMON

//
// Test code completion at the beginning of expr-postfix.
//

//===--- Helper types that are used in this test

struct FooStruct {
}

var fooObject : FooStruct

func fooFunc() -> FooStruct {
  return fooObject
}

enum FooEnum {
}

class FooClass {
}

protocol FooProtocol {
}

typealias FooTypealias = Int

// COMMON: Begin completions
// Function parameter
// COMMON-DAG: Decl[LocalVar]/Local: fooParam[#FooStruct#]{{$}}
// Global completions
// COMMON-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{$}}
// COMMON-DAG: Decl[Enum]/CurrModule:       FooEnum[#FooEnum#]{{$}}
// COMMON-DAG: Decl[Class]/CurrModule:      FooClass[#FooClass#]{{$}}
// COMMON-DAG: Decl[Protocol]/CurrModule:   FooProtocol[#FooProtocol#]{{$}}
// COMMON-DAG: Decl[TypeAlias]/CurrModule:  FooTypealias[#Int#]{{$}}
// COMMON-DAG: Decl[GlobalVar]/CurrModule:  fooObject[#FooStruct#]{{$}}
// COMMON-DAG: Keyword/None: true[#Bool#]{{$}}
// COMMON-DAG: Keyword/None: false[#Bool#]{{$}}
// COMMON-DAG: Keyword/None: nil{{$}}
// COMMON-DAG: Decl[Struct]/OtherModule:    Int8[#Int8#]{{$}}
// COMMON-DAG: Decl[Struct]/OtherModule:    Int16[#Int16#]{{$}}
// COMMON-DAG: Decl[Struct]/OtherModule:    Int32[#Int32#]{{$}}
// COMMON-DAG: Decl[Struct]/OtherModule:    Int64[#Int64#]{{$}}
// COMMON-DAG: Decl[Struct]/OtherModule:      Bool[#Bool#]{{$}}
// COMMON-DAG: Keyword/None: __FUNCTION__[#String#]{{$}}
// COMMON-DAG: Keyword/None: __FILE__[#String#]{{$}}
// COMMON-DAG: Keyword/None: __LINE__[#Int#]{{$}}
// COMMON-DAG: Keyword/None: __COLUMN__[#Int#]{{$}}
// COMMON: End completions

// NO_SELF-NOT: Self
// NO_SELF-NOT: self

//===--- Test that we can code complete at the beginning of expr-postfix.

func testExprPostfixBegin1(fooParam: FooStruct) {
  #^EXPR_POSTFIX_BEGIN_1^#
}

func testExprPostfixBegin2(fooParam: FooStruct) {
  1 + #^EXPR_POSTFIX_BEGIN_2^#
}

func testExprPostfixBegin3(fooParam: FooStruct) {
  fooFunc()
  1 + #^EXPR_POSTFIX_BEGIN_3^#
}

func testExprPostfixBegin4(fooParam: FooStruct) {
  "\(#^EXPR_POSTFIX_BEGIN_4^#)"
}

//===--- Test that we sometimes ignore the expr-postfix.
// In these cases, displaying '.instance*' completion results is technically
// valid, but would be extremely surprising.

func testExprPostfixBeginIgnored1(fooParam: FooStruct) {
  fooFunc()
  #^EXPR_POSTFIX_BEGIN_IGNORED_1^#
}

func testExprPostfixBeginIgnored2(fooParam: FooStruct) {
  123456789
  #^EXPR_POSTFIX_BEGIN_IGNORED_2^#
}

func testExprPostfixBeginIgnored3(fooParam: FooStruct) {
  123456789 +
      fooFunc()
  #^EXPR_POSTFIX_BEGIN_IGNORED_3^#
}

//===--- Test that we include function parameters in completion results.

func testFindFuncParam1(fooParam: FooStruct, a: Int, b: Float, inout c: Double)(inout d: Double) {
  #^FIND_FUNC_PARAM_1^#
// FIND_FUNC_PARAM_1: Begin completions
// FIND_FUNC_PARAM_1-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_FUNC_PARAM_1-DAG: Decl[LocalVar]/Local: b[#Float#]{{$}}
// FIND_FUNC_PARAM_1-DAG: Decl[LocalVar]/Local: c[#inout Double#]{{$}}
// FIND_FUNC_PARAM_1-DAG: Decl[LocalVar]/Local: d[#inout Double#]{{$}}
// FIND_FUNC_PARAM_1: End completions
}

func testFindFuncParam2<Foo : FooProtocol>(fooParam: FooStruct, foo: Foo) {
  #^FIND_FUNC_PARAM_2^#
// FIND_FUNC_PARAM_2: Begin completions
// FIND_FUNC_PARAM_2-DAG: Decl[GenericTypeParam]/Local: Foo[#Foo#]{{$}}
// FIND_FUNC_PARAM_2-DAG: Decl[LocalVar]/Local:         foo[#Foo#]{{$}}
// FIND_FUNC_PARAM_2: End completions
}

struct TestFindFuncParam3_4 {
  func testFindFuncParam3(a: Int, b: Float)(c: Double) {
    #^FIND_FUNC_PARAM_3^#
// FIND_FUNC_PARAM_3: Begin completions
// FIND_FUNC_PARAM_3-DAG: Decl[LocalVar]/Local: self[#TestFindFuncParam3_4#]{{$}}
// FIND_FUNC_PARAM_3-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_FUNC_PARAM_3-DAG: Decl[LocalVar]/Local: b[#Float#]{{$}}
// FIND_FUNC_PARAM_3-DAG: Decl[LocalVar]/Local: c[#Double#]{{$}}
// FIND_FUNC_PARAM_3: End completions
  }

  func testFindFuncParam4<U>(a: Int, b: U) {
    #^FIND_FUNC_PARAM_4^#
// FIND_FUNC_PARAM_4: Begin completions
// FIND_FUNC_PARAM_4-DAG: Decl[GenericTypeParam]/Local: U[#U#]{{$}}
// FIND_FUNC_PARAM_4-DAG: Decl[LocalVar]/Local:         self[#TestFindFuncParam3_4#]{{$}}
// FIND_FUNC_PARAM_4-DAG: Decl[LocalVar]/Local:         a[#Int#]{{$}}
// FIND_FUNC_PARAM_4-DAG: Decl[LocalVar]/Local:         b[#U#]{{$}}
// FIND_FUNC_PARAM_4: End completions
  }
}

struct TestFindFuncParam5_6<T> {
  func testFindFuncParam5(a: Int, b: T) {
    #^FIND_FUNC_PARAM_5^#
// FIND_FUNC_PARAM_5: Begin completions
// FIND_FUNC_PARAM_5-DAG: Decl[GenericTypeParam]/CurrNominal: T[#T#]{{$}}
// FIND_FUNC_PARAM_5-DAG: Decl[LocalVar]/Local: self[#TestFindFuncParam5_6<T>#]{{$}}
// FIND_FUNC_PARAM_5-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_FUNC_PARAM_5-DAG: Decl[LocalVar]/Local: b[#T#]{{$}}
// FIND_FUNC_PARAM_5: End completions
  }

  func testFindFuncParam6<U>(a: Int, b: T, c: U) {
    #^FIND_FUNC_PARAM_6^#
// FIND_FUNC_PARAM_6: Begin completions
// FIND_FUNC_PARAM_6-DAG: Decl[GenericTypeParam]/CurrNominal: T[#T#]{{$}}
// FIND_FUNC_PARAM_6-DAG: Decl[GenericTypeParam]/Local:       U[#U#]{{$}}
// FIND_FUNC_PARAM_6-DAG: Decl[LocalVar]/Local:               self[#TestFindFuncParam5_6<T>#]{{$}}
// FIND_FUNC_PARAM_6-DAG: Decl[LocalVar]/Local:               a[#Int#]{{$}}
// FIND_FUNC_PARAM_6-DAG: Decl[LocalVar]/Local:               b[#T#]{{$}}
// FIND_FUNC_PARAM_6-DAG: Decl[LocalVar]/Local:               c[#U#]{{$}}
// FIND_FUNC_PARAM_6: End completions
  }
}

class TestFindFuncParam7 {
  func testFindFuncParam7(a: Int, b: Float)(c: Double) {
    #^FIND_FUNC_PARAM_7^#
// FIND_FUNC_PARAM_7: Begin completions
// FIND_FUNC_PARAM_7-DAG: Decl[LocalVar]/Local: self[#TestFindFuncParam7#]{{$}}
// FIND_FUNC_PARAM_7-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_FUNC_PARAM_7-DAG: Decl[LocalVar]/Local: b[#Float#]{{$}}
// FIND_FUNC_PARAM_7-DAG: Decl[LocalVar]/Local: c[#Double#]{{$}}
// FIND_FUNC_PARAM_7: End completions
  }
}

func testFindFuncParamSelector1(a: Int, b x: Float, foo fooParam: FooStruct, inout bar barParam: FooStruct) {
  #^FIND_FUNC_PARAM_SELECTOR_1^#
// FIND_FUNC_PARAM_SELECTOR_1: Begin completions
// FIND_FUNC_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_FUNC_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: x[#Float#]{{$}}
// FIND_FUNC_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: barParam[#inout FooStruct#]{{$}}
// FIND_FUNC_PARAM_SELECTOR_1: End completions
}

//===--- Test that we include constructor parameters in completion results.

class TestFindConstructorParam1 {
  init(a: Int, b: Float) {
    #^FIND_CONSTRUCTOR_PARAM_1^#
// FIND_CONSTRUCTOR_PARAM_1: Begin completions
// FIND_CONSTRUCTOR_PARAM_1-DAG: Decl[LocalVar]/Local: self[#TestFindConstructorParam1#]{{$}}
// FIND_CONSTRUCTOR_PARAM_1-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_1-DAG: Decl[LocalVar]/Local: b[#Float#]{{$}}
// FIND_CONSTRUCTOR_PARAM_1: End completions
  }
}

struct TestFindConstructorParam2 {
  init(a: Int, b: Float) {
    #^FIND_CONSTRUCTOR_PARAM_2^#
// FIND_CONSTRUCTOR_PARAM_2: Begin completions
// FIND_CONSTRUCTOR_PARAM_2-DAG: Decl[LocalVar]/Local: self[#TestFindConstructorParam2#]{{$}}
// FIND_CONSTRUCTOR_PARAM_2-DAG: Decl[LocalVar]/Local: a[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_2-DAG: Decl[LocalVar]/Local: b[#Float#]{{$}}
// FIND_CONSTRUCTOR_PARAM_2: End completions
  }
}

class TestFindConstructorParam3 {
  init<U>(a: Int, b: U) {
    #^FIND_CONSTRUCTOR_PARAM_3^#
// FIND_CONSTRUCTOR_PARAM_3: Begin completions
// FIND_CONSTRUCTOR_PARAM_3-DAG: Decl[GenericTypeParam]/Local: U[#U#]{{$}}
// FIND_CONSTRUCTOR_PARAM_3-DAG: Decl[LocalVar]/Local:         self[#TestFindConstructorParam3#]{{$}}
// FIND_CONSTRUCTOR_PARAM_3-DAG: Decl[LocalVar]/Local:         a[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_3-DAG: Decl[LocalVar]/Local:         b[#U#]{{$}}
// FIND_CONSTRUCTOR_PARAM_3: End completions
  }
}

class TestFindConstructorParam4<T> {
  init(a: Int, b: T) {
    #^FIND_CONSTRUCTOR_PARAM_4^#
// FIND_CONSTRUCTOR_PARAM_4: Begin completions
// FIND_CONSTRUCTOR_PARAM_4-DAG: Decl[GenericTypeParam]/CurrNominal: T[#T#]{{$}}
// FIND_CONSTRUCTOR_PARAM_4-DAG: Decl[LocalVar]/Local:               self[#TestFindConstructorParam4<T>#]{{$}}
// FIND_CONSTRUCTOR_PARAM_4-DAG: Decl[LocalVar]/Local:               a[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_4-DAG: Decl[LocalVar]/Local:               b[#T#]{{$}}
// FIND_CONSTRUCTOR_PARAM_4: End completions
  }
}

class TestFindConstructorParam5<T> {
  init<U>(a: Int, b: T, c: U) {
    #^FIND_CONSTRUCTOR_PARAM_5^#
// FIND_CONSTRUCTOR_PARAM_5: Begin completions
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[GenericTypeParam]/CurrNominal: T[#T#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[GenericTypeParam]/Local:       U[#U#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[LocalVar]/Local:               self[#TestFindConstructorParam5<T>#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[LocalVar]/Local:               a[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[LocalVar]/Local:               b[#T#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5-DAG: Decl[LocalVar]/Local:               c[#U#]{{$}}
// FIND_CONSTRUCTOR_PARAM_5: End completions
  }
}

class TestFindConstructorParamSelector1 {
  init(a x: Int, b y: Float) {
    #^FIND_CONSTRUCTOR_PARAM_SELECTOR_1^#
// FIND_CONSTRUCTOR_PARAM_SELECTOR_1: Begin completions
// FIND_CONSTRUCTOR_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: self[#TestFindConstructorParamSelector1#]{{$}}
// FIND_CONSTRUCTOR_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: x[#Int#]{{$}}
// FIND_CONSTRUCTOR_PARAM_SELECTOR_1-DAG: Decl[LocalVar]/Local: y[#Float#]{{$}}
// FIND_CONSTRUCTOR_PARAM_SELECTOR_1: End completions
  }
}

//===--- Test that we include destructor's 'self' in completion results.

class TestFindDestructorParam1 {
  deinit {
    #^FIND_DESTRUCTOR_PARAM_1^#
// FIND_DESTRUCTOR_PARAM_1: Begin completions
// FIND_DESTRUCTOR_PARAM_1-DAG: Decl[LocalVar]/Local: self[#TestFindDestructorParam1#]{{$}}
// FIND_DESTRUCTOR_PARAM_1: End completions
  }
}

class TestFindDestructorParam2<T> {
  deinit {
    #^FIND_DESTRUCTOR_PARAM_2^#
// FIND_DESTRUCTOR_PARAM_2: Begin completions
// FIND_DESTRUCTOR_PARAM_2-DAG: Decl[GenericTypeParam]/CurrNominal: T[#T#]{{$}}
// FIND_DESTRUCTOR_PARAM_2-DAG: Decl[LocalVar]/Local: self[#TestFindDestructorParam2<T>#]{{$}}
// FIND_DESTRUCTOR_PARAM_2: End completions
  }
}

//===--- Test that we don't crash in constructors and destructors in contexts
//===--- where they are not allowed.

init() {
  var fooParam = FooStruct()
  #^IN_INVALID_1^#
}

init { // Missing parameters
  var fooParam = FooStruct()
  #^IN_INVALID_2^#
}

deinit {
  var fooParam = FooStruct()
  #^IN_INVALID_3^#
}

func testInInvalid5() {
  var fooParam = FooStruct()
  init() {
    #^IN_INVALID_5^#
  }
}

func testInInvalid6() {
  deinit {
    var fooParam = FooStruct()
    #^IN_INVALID_6^#
  }
}

struct TestInInvalid7 {
  deinit {
    var fooParam = FooStruct()
    #^IN_INVALID_7^#
  }
}

func foo() -> Undeclared {
  var fooParam = FooStruct()
  #^IN_INVALID_8^#
}
