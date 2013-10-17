// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_SW_1 > %t.enum.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_RESULTS < %t.enum.txt
// RUN: FileCheck %s -check-prefix=FOO_ENUM_TYPE_CONTEXT < %t.enum.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_SW_2 > %t.enum.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_RESULTS < %t.enum.txt
// RUN: FileCheck %s -check-prefix=FOO_ENUM_TYPE_CONTEXT < %t.enum.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_SW_3 > %t.enum.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_RESULTS < %t.enum.txt
// RUN: FileCheck %s -check-prefix=BAR_ENUM_TYPE_CONTEXT < %t.enum.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_SW_4 > %t.enum.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_RESULTS < %t.enum.txt
// RUN: FileCheck %s -check-prefix=BAZ_ENUM_TYPE_CONTEXT < %t.enum.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_SW_5 > %t.enum.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_RESULTS < %t.enum.txt
// RUN: FileCheck %s -check-prefix=QUX_ENUM_TYPE_CONTEXT < %t.enum.txt

//===---
//===--- Test that we can complete enum elements.
//===---

//===--- Helper types.

enum FooEnum {
  case Foo1
  case Foo2
}

// FOO_ENUM_TYPE_CONTEXT: Begin completions
// FOO_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Foo1[#FooEnum.metatype -> FooEnum#]{{$}}
// FOO_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Foo2[#FooEnum.metatype -> FooEnum#]{{$}}
// FOO_ENUM_TYPE_CONTEXT: End completions

enum BarEnum {
  case Bar1
  case Bar2()
  case Bar3(a: Int)
  case Bar4(a: Int, b: Float)
  case Bar5(a: Int, (b: Float))
  case Bar6(a: Int, b: (c: Float))
  case Bar7(a: Int, (b: Float, c: Double))
  case Bar8(a: Int, b: (c: Float, d: Double))
  case Bar9(Int)
  case Bar10(Int, Float)
  case Bar11(Int, (Float))
  case Bar12(Int, (Float, Double))

  func barInstanceFunc() {}
  static func barStaticFunc() {}
}

// BAR_ENUM_TYPE_CONTEXT: Begin completions
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar1[#BarEnum.metatype -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar2()[#BarEnum.metatype -> () -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar3({#a: Int#})[#BarEnum.metatype -> (a: Int) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar4({#a: Int#}, {#b: Float#})[#BarEnum.metatype -> (a: Int, b: Float) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar5({#a: Int#}, ({#b: Float#}))[#BarEnum.metatype -> (a: Int, (b: Float)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar6({#a: Int#}, b: ({#c: Float#}))[#BarEnum.metatype -> (a: Int, b: (c: Float)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar7({#a: Int#}, ({#b: Float#}, {#c: Double#}))[#BarEnum.metatype -> (a: Int, (b: Float, c: Double)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar8({#a: Int#}, b: ({#c: Float#}, {#d: Double#}))[#BarEnum.metatype -> (a: Int, b: (c: Float, d: Double)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar9{#Int#}[#BarEnum.metatype -> (Int) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar10({#Int#}, {#Float#})[#BarEnum.metatype -> (Int, Float) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar11({#Int#}, {#Float#})[#BarEnum.metatype -> (Int, (Float)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Bar12({#Int#}, ({#Float#}, {#Double#}))[#BarEnum.metatype -> (Int, (Float, Double)) -> BarEnum#]{{$}}
// BAR_ENUM_TYPE_CONTEXT: End completions

enum BazEnum<T> {
  case Baz1
  case Baz2(T)

  func bazInstanceFunc() {}
  static func bazStaticFunc() {}
}

// BAZ_ENUM_TYPE_CONTEXT: Begin completions
// BAZ_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Baz1[#<T> BazEnum<T>.metatype -> BazEnum<T>#]{{$}}
// BAZ_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Baz2{#T#}[#<T> BazEnum<T>.metatype -> (T) -> BazEnum<T>#]{{$}}
// BAZ_ENUM_TYPE_CONTEXT: End completions

enum QuxEnum : Int {
  case Qux1 = 10
  case Qux2 = 20
}

// QUX_ENUM_TYPE_CONTEXT: Begin completions
// QUX_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Qux1[#QuxEnum.metatype -> QuxEnum#]{{$}}
// QUX_ENUM_TYPE_CONTEXT-DAG: Decl/ExprSpecific: .Qux2[#QuxEnum.metatype -> QuxEnum#]{{$}}
// QUX_ENUM_TYPE_CONTEXT: End completions

func freeFunc() {}

// WITH_GLOBAL_RESULTS: Begin completions
// WITH_GLOBAL_RESULTS: Decl/CurrModule: freeFunc()[#Void#]{{$}}
// WITH_GLOBAL_RESULTS: End completions

//===--- Complete enum elements in 'switch'.

func testSwitch1(e: FooEnum) {
  switch e {
  case #^ENUM_SW_1^#
  }
}
func testSwitch2(e: FooEnum) {
  switch e {
  case .Foo1:
  case #^ENUM_SW_2^#
  }
}
func testSwitch3(e: BarEnum) {
  switch e {
  case #^ENUM_SW_3^#
  }
}
func testSwitch4(e: BazEnum<Int>) {
  switch e {
  case #^ENUM_SW_4^#
  }
}
func testSwitch5(e: QuxEnum) {
  switch e {
  case #^ENUM_SW_5^#
  }
}

