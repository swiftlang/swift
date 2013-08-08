// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_1 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_1
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_2 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_2
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_3 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_3
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_4 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_4
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_5 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_5
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_6 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_6
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_7 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_7
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_8 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_8
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_9 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_9

// Test code completion at the beginning of expr-postfix.

struct FooStruct {
}

var fooObject : FooStruct

union FooUnion {
}

class FooClass {
}

protocol FooProtocol {
}

typealias FooTypealias = Int

func testExprPostfixBeginA() {
  #^EXPR_POSTFIX_BEGIN_1^#
// EXPR_POSTFIX_BEGIN_1: Begin completions
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooClass[#FooClass.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: FooTypealias[#Int.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: true[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: false[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int8[#Int8.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int16[#Int16.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int32[#Int32.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int64[#Int64.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Int128[#Int128.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: SwiftDecl: Bool[#Bool.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: Keyword: __FILE__[#String#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: Keyword: __LINE__[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_1-DAG: Keyword: __COLUMN__[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_1: End completions

  1 + #^EXPR_POSTFIX_BEGIN_2^#
// EXPR_POSTFIX_BEGIN_2: Begin completions
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooClass[#FooClass.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: FooTypealias[#Int.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: true[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: false[#Bool#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int8[#Int8.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int16[#Int16.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int32[#Int32.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int64[#Int64.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Int128[#Int128.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: SwiftDecl: Bool[#Bool.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: Keyword: __FILE__[#String#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: Keyword: __LINE__[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_2-DAG: Keyword: __COLUMN__[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_2: End completions
}

func testExprPostfixBeginB(a : Int, b : Float)(c : Double) {
  #^EXPR_POSTFIX_BEGIN_3^#
// EXPR_POSTFIX_BEGIN_3: Begin completions
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_3-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_3: End completions
}

func testExprPostfixBeginC<Foo : FooProtocol>(foo : Foo) {
  #^EXPR_POSTFIX_BEGIN_4^#
// EXPR_POSTFIX_BEGIN_4: Begin completions
// EXPR_POSTFIX_BEGIN_4-DAG: SwiftDecl: Foo[#Foo.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_4-DAG: SwiftDecl: foo[#Foo#]{{$}}
// EXPR_POSTFIX_BEGIN_4: End completions
}

struct TestExprPostfixBeginD {
  func testExprPostfixBeginE(a : Int, b : Float)(c : Double) {
    #^EXPR_POSTFIX_BEGIN_5^#
// EXPR_POSTFIX_BEGIN_5: Begin completions
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: this[#[byref] TestExprPostfixBeginD#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_5-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_5: End completions
  }

  func testExprPostfixBeginF<U>(a : Int, b : U) {
    #^EXPR_POSTFIX_BEGIN_6^#
// EXPR_POSTFIX_BEGIN_6: Begin completions
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: U[#U.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_6-DAG: SwiftDecl: b[#U#]{{$}}
// EXPR_POSTFIX_BEGIN_6: End completions
  }
}

struct TestExprPostfixBeginG<T> {
  func testExprPostfixBeginH(a : Int, b : T) {
    #^EXPR_POSTFIX_BEGIN_7^#
// EXPR_POSTFIX_BEGIN_7: Begin completions
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: T[#T.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_7-DAG: SwiftDecl: b[#T#]{{$}}
// EXPR_POSTFIX_BEGIN_7: End completions
  }

  func testExprPostfixBeginI<U>(a : Int, b : T, c : U) {
    #^EXPR_POSTFIX_BEGIN_8^#
// EXPR_POSTFIX_BEGIN_8: Begin completions
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: T[#T.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: U[#U.metatype#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: b[#T#]{{$}}
// EXPR_POSTFIX_BEGIN_8-DAG: SwiftDecl: c[#U#]{{$}}
// EXPR_POSTFIX_BEGIN_8: End completions
  }
}

class TestExprPostfixBeginJ {
  func testExprPostfixBeginK(a : Int, b : Float)(c : Double) {
    #^EXPR_POSTFIX_BEGIN_9^#
// EXPR_POSTFIX_BEGIN_9: Begin completions
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: this[#TestExprPostfixBeginJ#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: a[#Int#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: b[#Float#]{{$}}
// EXPR_POSTFIX_BEGIN_9-DAG: SwiftDecl: c[#Double#]{{$}}
// EXPR_POSTFIX_BEGIN_9: End completions
  }
}

