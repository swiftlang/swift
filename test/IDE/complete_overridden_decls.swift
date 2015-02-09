// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_BASE_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_BASE_1 < %t.over.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_DERIVED_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_DERIVED_1 < %t.over.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVER_MORE_DERIVED_1 > %t.over.txt
// RUN: FileCheck %s -check-prefix=OVER_MORE_DERIVED_1 < %t.over.txt

//===---
//===--- Check that we don't show overridden decls (only show the overridding decl).
//===---

class FooBase {}
class FooDerived : FooBase {}
class FooMoreDerived : FooDerived {}

class TestABase {
  var baseInstanceVar: FooBase { return FooBase() }

  var baseOverInstanceVar: FooBase { return FooBase() }

  func baseOverFunc() {}
  func baseOverContravariant(a: FooMoreDerived) {}
  func baseOverCovariant() -> FooBase {}
}
class TestADerived : TestABase {
  var derivedInstanceVar: FooBase { return FooBase() }

  override var baseOverInstanceVar: FooDerived { return FooDerived() }
  var derivedOverInstanceVar: FooBase { return FooBase() }

  override func baseOverFunc() {}
  override func baseOverContravariant(a: FooDerived) {}
  override func baseOverCovariant() -> FooDerived {}
}
class TestAMoreDerived : TestADerived {
  var moreDerivedInstanceVar: FooBase { return FooBase() }

  override var baseOverInstanceVar: FooMoreDerived { return FooMoreDerived() }
  override var derivedOverInstanceVar: FooDerived { return FooDerived() }

  override func baseOverFunc() {}
  override func baseOverContravariant(a: FooBase) {}
  override func baseOverCovariant() -> FooMoreDerived {}
}

// NO_ERRORS_UP_TO_HERE

func test1(b: TestABase) {
  b.#^OVER_BASE_1^#
}
// OVER_BASE_1: Begin completions
// OVER_BASE_1-NEXT: Decl[InstanceVar]/CurrNominal:    baseInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_BASE_1-NEXT: Decl[InstanceVar]/CurrNominal:    baseOverInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_BASE_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverFunc()[#Void#]{{; name=.+$}}
// OVER_BASE_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverContravariant({#(a): FooMoreDerived#})[#Void#]{{; name=.+$}}
// OVER_BASE_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverCovariant()[#FooBase#]{{; name=.+$}}
// OVER_BASE_1-NEXT: End completions

func test2(d: TestADerived) {
  d.#^OVER_DERIVED_1^#
}
// OVER_DERIVED_1: Begin completions
// OVER_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    derivedInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    baseOverInstanceVar[#FooDerived#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    derivedOverInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverFunc()[#Void#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverContravariant({#(a): FooDerived#})[#Void#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverCovariant()[#FooDerived#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: Decl[InstanceVar]/Super:          baseInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_DERIVED_1-NEXT: End completions

func test3(md: TestAMoreDerived) {
  md.#^OVER_MORE_DERIVED_1^#
}
// OVER_MORE_DERIVED_1: Begin completions
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    moreDerivedInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    baseOverInstanceVar[#FooMoreDerived#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceVar]/CurrNominal:    derivedOverInstanceVar[#FooDerived#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverFunc()[#Void#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverContravariant({#(a): FooBase#})[#Void#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceMethod]/CurrNominal: baseOverCovariant()[#FooMoreDerived#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceVar]/Super:          derivedInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: Decl[InstanceVar]/Super:          baseInstanceVar[#FooBase#]{{; name=.+$}}
// OVER_MORE_DERIVED_1-NEXT: End completions
