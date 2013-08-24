// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_1 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_2 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_3 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_4 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_5 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_6 | FileCheck %s -check-prefix=ERROR_COMMON

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
  var fooInstanceVar : Int
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a : Int) -> Double
  subscript(i: Int) -> Double
}

protocol BarProtocol {
  var barInstanceVar : Int
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a : Int) -> Double
}

typealias FooTypealias = Int

// WITH_GLOBAL_TYPES: Begin completions
// Global completions
// WITH_GLOBAL_TYPES-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// WITH_GLOBAL_TYPES-DAG: SwiftDecl: FooUnion[#FooUnion.metatype#]{{$}}
// WITH_GLOBAL_TYPES-DAG: SwiftDecl: FooClass[#FooClass.metatype#]{{$}}
// WITH_GLOBAL_TYPES-DAG: SwiftDecl: FooProtocol[#FooProtocol.metatype#]{{$}}
// WITH_GLOBAL_TYPES-DAG: SwiftDecl: FooTypealias[#Int.metatype#]{{$}}
// WITH_GLOBAL_TYPES: End completions

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

var (a : #^PATTERN_TUPLE_1^#
var (a : Int, b : #^PATTERN_TUPLE_2^#
var (a :, b : #^PATTERN_TUPLE_3^#
var (a: b: #^PATTERN_TUPLE_4^#

//===--- Test that we don't try to suggest anything where pattern-atom is expected.

var #^PATTERN_ATOM_1^#
var (#^PATTERN_ATOM_2^#
var (a, #^PATTERN_ATOM_3^#
var (a #^PATTERN_ATOM_4^#
var ((#^PATTERN_ATOM_5^#
var ((a, b), #^PATTERN_ATOM_6^#

