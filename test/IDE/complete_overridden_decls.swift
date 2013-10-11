// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_BASE_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_BASE_1 < %t.over.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_DERIVED_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_DERIVED_1 < %t.over.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_MORE_DERIVED_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_MORE_DERIVED_1 < %t.over.txt

//===---
//===--- Check that we don't show overridden decls (only show the overridding decl).
//===---

class FooBase {}
class FooDerived : FooBase {}
class FooMoreDerived : FooDerived {}

class TestABase {
  var baseInstanceVar: FooBase { get: return FooBase() }

  var baseOverInstanceVar: FooBase { get: return FooBase() }

  func baseOverFunc() {}
  func baseOverContravariant(a: FooMoreDerived) {}
  func baseOverCovariant() -> FooBase {}
}
class TestADerived : TestABase {
  var derivedInstanceVar: FooBase { get: return FooBase() }

  var baseOverInstanceVar: FooDerived { get: return FooDerived() }
  var derivedOverInstanceVar: FooBase { get: return FooBase() }

  func baseOverFunc() {}
  func baseOverContravariant(a: FooDerived) {}
  func baseOverCovariant() -> FooDerived {}
}
class TestAMoreDerived : TestABase {
  var moreDerivedInstanceVar: FooBase { get: return FooBase() }

  var baseOverInstanceVar: FooMoreDerived { get: return FooMoreDerived() }
  var derivedOverInstanceVar: FooDerived { get: return FooDerived() }

  func baseOverFunc() {}
  func baseOverContravariant(a: FooBase) {}
  func baseOverCovariant() -> FooMoreDerived {}
}

func test1(b: TestABase) {
  b.#^OVER_BASE_1^#
}
// OVER_BASE_1: Begin completions, 6 items
// OVER_BASE_1-NEXT: Decl/CurrNominal: baseInstanceVar[#FooBase#]{{$}}
// OVER_BASE_1-NEXT: Decl/CurrNominal: baseOverInstanceVar[#FooBase#]{{$}}
// OVER_BASE_1-NEXT: Decl/CurrNominal: baseOverFunc()[#Void#]{{$}}
// OVER_BASE_1-NEXT: Decl/CurrNominal: baseOverContravariant({#a: FooMoreDerived#})[#Void#]{{$}}
// OVER_BASE_1-NEXT: Decl/CurrNominal: baseOverCovariant()[#FooBase#]{{$}}
// OVER_BASE_1-NEXT: Keyword/None:     metatype[#TestABase.metatype#]{{$}}
// OVER_BASE_1-NEXT: End completions

func test2(d: TestADerived) {
  d.#^OVER_DERIVED_1^#
}
// OVER_DERIVED_1: Begin completions, 8 items
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: derivedInstanceVar[#FooBase#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: baseOverInstanceVar[#FooDerived#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: derivedOverInstanceVar[#FooBase#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: baseOverFunc()[#Void#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: baseOverContravariant({#a: FooDerived#})[#Void#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/CurrNominal: baseOverCovariant()[#FooDerived#]{{$}}
// OVER_DERIVED_1-NEXT: Decl/Super:       baseInstanceVar[#FooBase#]{{$}}
// OVER_DERIVED_1-NEXT: Keyword/None:     metatype[#TestADerived.metatype#]{{$}}
// OVER_DERIVED_1-NEXT: End completions

func test3(md: TestAMoreDerived) {
  md.#^OVER_MORE_DERIVED_1^#
}
// OVER_MORE_DERIVED_1: Begin completions, 8 items
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: moreDerivedInstanceVar[#FooBase#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: baseOverInstanceVar[#FooMoreDerived#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: derivedOverInstanceVar[#FooDerived#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: baseOverFunc()[#Void#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: baseOverContravariant({#a: FooBase#})[#Void#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/CurrNominal: baseOverCovariant()[#FooMoreDerived#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Decl/Super:       baseInstanceVar[#FooBase#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: Keyword/None:     metatype[#TestAMoreDerived.metatype#]{{$}}
// OVER_MORE_DERIVED_1-NEXT: End completions

