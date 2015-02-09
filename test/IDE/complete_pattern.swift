// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_TUPLE_4 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_1 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_2 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_3 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_4 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_5 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_ATOM_6 | FileCheck %s -check-prefix=ERROR_COMMON


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_IS_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_IS_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_IS_3 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_IS_GENERIC_1 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt
// RUN: FileCheck %s -check-prefix=PATTERN_IS_GENERIC_1 < %t.types.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PATTERN_IS_GENERIC_2 > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.types.txt
// RUN: FileCheck %s -check-prefix=PATTERN_IS_GENERIC_2 < %t.types.txt

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
  var fooInstanceVar : Int
  typealias FooTypeAlias1
  func fooInstanceFunc0() -> Double
  func fooInstanceFunc1(a: Int) -> Double
  subscript(i: Int) -> Double
}

protocol BarProtocol {
  var barInstanceVar : Int
  typealias BarTypeAlias1
  func barInstanceFunc0() -> Double
  func barInstanceFunc1(a: Int) -> Double
}

typealias FooTypealias = Int

// WITH_GLOBAL_TYPES: Begin completions
// Global completions
// WITH_GLOBAL_TYPES-DAG: Decl[Struct]/CurrModule:    FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Enum]/CurrModule:      FooEnum[#FooEnum#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Class]/CurrModule:     FooClass[#FooClass#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[Protocol]/CurrModule:  FooProtocol[#FooProtocol#]{{; name=.+$}}
// WITH_GLOBAL_TYPES-DAG: Decl[TypeAlias]/CurrModule: FooTypealias[#Int#]{{; name=.+$}}
// WITH_GLOBAL_TYPES: End completions

// GLOBAL_NEGATIVE-NOT: fooObject
// GLOBAL_NEGATIVE-NOT: fooFunc

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

var (a : #^PATTERN_TUPLE_1^#;
var (a : Int, b : #^PATTERN_TUPLE_2^#;
var (a :, b : #^PATTERN_TUPLE_3^#;
var (a: b: #^PATTERN_TUPLE_4^#;

//===---
//===--- Test that we don't try to suggest anything where pattern-atom is expected.
//===---

var #^PATTERN_ATOM_1^#
var (#^PATTERN_ATOM_2^#
var (a, #^PATTERN_ATOM_3^#
var (a #^PATTERN_ATOM_4^#
var ((#^PATTERN_ATOM_5^#
var ((a, b), #^PATTERN_ATOM_6^#

//===---
//===--- Test that we complete the type in 'is' pattern.
//===---

func patternIs1(x: FooClass) {
  switch x {
  case is #^PATTERN_IS_1^#
  }
}

func patternIs2() {
  switch unknown_var {
  case is #^PATTERN_IS_2^#
  }
}

func patternIs3() {
  switch {
  case is #^PATTERN_IS_3^#
  }
}

//===--- Test that we include types from generic parameter lists.

func patternIsGeneric1<
    GenericFoo : FooProtocol,
    GenericBar : protocol<FooProtocol, BarProtocol>,
    GenericBaz>(x: FooClass) {
  switch x {
  case is #^PATTERN_IS_GENERIC_1^#
  }
}

// PATTERN_IS_GENERIC_1: Begin completions
// Generic parameters of the function.
// PATTERN_IS_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// PATTERN_IS_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// PATTERN_IS_GENERIC_1-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// PATTERN_IS_GENERIC_1: End completions

struct PatternIsGeneric2<
    StructGenericFoo : FooProtocol,
    StructGenericBar : protocol<FooProtocol, BarProtocol>,
    StructGenericBaz> {
  func patternIsGeneric2<
      GenericFoo : FooProtocol,
      GenericBar : protocol<FooProtocol, BarProtocol>,
      GenericBaz>(x: FooClass) {
    switch x {
    case is #^PATTERN_IS_GENERIC_2^#
    }
  }
}

// PATTERN_IS_GENERIC_2: Begin completions
// Generic parameters of the struct.
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericFoo[#StructGenericFoo#]{{; name=.+$}}
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBar[#StructGenericBar#]{{; name=.+$}}
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/CurrNominal: StructGenericBaz[#StructGenericBaz#]{{; name=.+$}}
// Generic parameters of the function.
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericFoo[#GenericFoo#]{{; name=.+$}}
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericBar[#GenericBar#]{{; name=.+$}}
// PATTERN_IS_GENERIC_2-DAG: Decl[GenericTypeParam]/Local: GenericBaz[#GenericBaz#]{{; name=.+$}}
// PATTERN_IS_GENERIC_2: End completions
