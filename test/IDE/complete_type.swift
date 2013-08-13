// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_FUNC_PARAM_5 | FileCheck %s -check-prefix=ERROR_COMMON

//===--- Helper types that are used in this test

struct FooStruct {
}

var fooObject : FooStruct

func fooFunc() -> FooStruct {
  return fooObject
}

union FooUnion {
}

class FooClass {
}

protocol FooProtocol {
}

typealias FooTypealias = Int

// GLOBAL_TYPES: Begin completions
// Global completions
// GLOBAL_TYPES-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]
// GLOBAL_TYPES-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]
// GLOBAL_TYPES-DAG: SwiftDecl: FooClass[#FooClass.metatype#]
// GLOBAL_TYPES-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]
// GLOBAL_TYPES-DAG: SwiftDecl: FooTypealias[#Int.metatype#]
// GLOBAL_TYPES: End completions

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

//===--- Test that we can complete types inside function parameters.

func testTypeInParam1(a: #^TYPE_IN_FUNC_PARAM_1^#

func testTypeInParam2(a: Int, b: #^TYPE_IN_FUNC_PARAM_2^#

func testTypeInParam3(a: unknown_type, b: #^TYPE_IN_FUNC_PARAM_3^#

func testTypeInParam4(a: , b: #^TYPE_IN_FUNC_PARAM_4^#

// FIXME: Right now we don't provide any completions.
// FIXME: Improve parser recovery.
func testTypeInParam5(a: b: #^TYPE_IN_FUNC_PARAM_5^#

// TODO: Also consider generic arguments

