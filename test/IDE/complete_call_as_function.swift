// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_NO_DOT | %FileCheck %s -check-prefix=INSTANCE_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_DOT | %FileCheck %s -check-prefix=INSTANCE_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_PAREN | %FileCheck %s -check-prefix=INSTANCE_PAREN
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INSTANCE_ARG2 | %FileCheck %s -check-prefix=INSTANCE_ARG2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_NO_DOT | %FileCheck %s -check-prefix=METATYPE_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_DOT | %FileCheck %s -check-prefix=METATYPE_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METATYPE_PAREN | %FileCheck %s -check-prefix=METATYPE_PAREN
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPEEXPR_NO_DOT | %FileCheck %s -check-prefix=TYPEEXPR_NO_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPEEXPR_DOT | %FileCheck %s -check-prefix=TYPEEXPR_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPEEXPR_PAREN | %FileCheck %s -check-prefix=TYPEEXPR_PAREN

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_PAREN | %FileCheck %s -check-prefix=OVERLOADED_PAREN
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_ARG2_LABEL | %FileCheck %s -check-prefix=OVERLOADED_ARG2_LABEL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_ARG2_VALUE | %FileCheck %s -check-prefix=OVERLOADED_ARG2_VALUE

struct Adder {
  private var base: Int
  init(base: Int) { self.base = base }
  func callAsFunction(x : Int, y : Int) -> Int { base + x + y }
}
func testCallAsFunction(add: Adder, addTy: Adder.Type) {
  let _ = add#^INSTANCE_NO_DOT^#;
// INSTANCE_NO_DOT: Begin completions, 3 items
// INSTANCE_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   .callAsFunction({#x: Int#}, {#y: Int#})[#Int#];
// INSTANCE_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   ({#x: Int#}, {#y: Int#})[#Int#];
// INSTANCE_NO_DOT-DAG: Keyword[self]/CurrNominal:          .self[#Adder#];
// INSTANCE_NO_DOT: End completions

  let _ = add.#^INSTANCE_DOT^#;
// INSTANCE_DOT: Begin completions, 2 items
// INSTANCE_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   callAsFunction({#x: Int#}, {#y: Int#})[#Int#];
// INSTANCE_DOT-DAG: Keyword[self]/CurrNominal:          self[#Adder#];
// INSTANCE_DOT: End completions

  let _ = add(#^INSTANCE_PAREN^#)
// INSTANCE_PAREN: Begin completions, 1 items
// INSTANCE_PAREN-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#x: Int#}, {#y: Int#}[')'][#Int#];
// INSTANCE_PAREN: End completions

  let _ = add(x: 12, #^INSTANCE_ARG2^#)
// INSTANCE_ARG2: Begin completions, 1 items
// INSTANCE_ARG2: Pattern/ExprSpecific:               {#y: Int#}[#Int#];
// INSTANCE_ARG2: End completions

  let _ = addTy#^METATYPE_NO_DOT^#;
// METATYPE_NO_DOT: Begin completions, 3 items
// METATYPE_NO_DOT-NOT: {#x: Int#}, {#y: Int#}
// METATYPE_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   .callAsFunction({#(self): Adder#})[#(x: Int, y: Int) -> Int#];
// METATYPE_NO_DOT-DAG: Decl[Constructor]/CurrNominal:      .init({#base: Int#})[#Adder#];
// METATYPE_NO_DOT-DAG: Keyword[self]/CurrNominal:          .self[#Adder.Type#];
// METATYPE_NO_DOT: End completions

  let _ = addTy.#^METATYPE_DOT^#;
// METATYPE_DOT: Begin completions, 3 items
// METATYPE_DOT-NOT: {#x: Int#}, {#y: Int#}
// METATYPE_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   callAsFunction({#(self): Adder#})[#(x: Int, y: Int) -> Int#];
// METATYPE_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#base: Int#})[#Adder#];
// METATYPE_DOT-DAG: Keyword[self]/CurrNominal:          self[#Adder.Type#];
// METATYPE_DOT: End completions

  let _ = addTy(#^METATYPE_PAREN^#)
// METATYPE_PAREN: Begin completions
// METATYPE_PAREN-NOT: {#x: Int#}, {#y: Int#}
// METATYPE_PAREN-NOT: {#base: Int#}
// METATYPE_PAREN: End completions

  let _ = Adder#^TYPEEXPR_NO_DOT^#;
// TYPEEXPR_NO_DOT: Begin completions, 4 items
// TYPEEXPR_NO_DOT-NOT: {#x: Int#}, {#y: Int#}
// TYPEEXPR_NO_DOT-DAG: Decl[Constructor]/CurrNominal:      ({#base: Int#})[#Adder#]; name=(base: Int)
// TYPEEXPR_NO_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   .callAsFunction({#(self): Adder#})[#(x: Int, y: Int) -> Int#];
// TYPEEXPR_NO_DOT-DAG: Keyword[self]/CurrNominal:          .self[#Adder.Type#];
// TYPEEXPR_NO_DOT-DAG: Keyword/CurrNominal:                .Type[#Adder.Type#];
// TYPEEXPR_NO_DOT: End completions

  let _ = Adder.#^TYPEEXPR_DOT^#;
// TYPEEXPR_DOT: Begin completions, 4 items
// TYPEEXPR_DOT-NOT: {#x: Int#}, {#y: Int#}
// TYPEEXPR_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   callAsFunction({#(self): Adder#})[#(x: Int, y: Int) -> Int#];
// TYPEEXPR_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#base: Int#})[#Adder#];
// TYPEEXPR_DOT-DAG: Keyword[self]/CurrNominal:          self[#Adder.Type#];
// TYPEEXPR_DOT-DAG: Keyword/CurrNominal:                Type[#Adder.Type#];
// TYPEEXPR_DOT: End completions

  let _ = Adder(#^TYPEEXPR_PAREN^#)
// TYPEEXPR_PAREN: Begin completions, 1 items
// TYPEEXPR_PAREN-NOT: {#x: Int#}, {#y: Int#}
// TYPEEXPR_PAREN-DAG: Decl[Constructor]/CurrNominal:      ['(']{#base: Int#}[')'][#Adder#];
// TYPEEXPR_PAREN: End completions
}

struct Functor {
  enum Horizontal { case left, right }
  enum Vertical { case up, down }
  func callAsFunction(h: Horizontal, v: Vertical) {}
  func callAsFunction(v: Vertical, h: Horizontal) {}
}
func testCallAsFunctionOverloaded(fn: Functor) {
  fn(#^OVERLOADED_PAREN^#)
//OVERLOADED_PAREN: Begin completions, 2 items
//OVERLOADED_PAREN-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#h: Functor.Horizontal#}, {#v: Functor.Vertical#}[')'][#Void#];
//OVERLOADED_PAREN-DAG: Decl[InstanceMethod]/CurrNominal:   ['(']{#v: Functor.Vertical#}, {#h: Functor.Horizontal#}[')'][#Void#];
//OVERLOADED_PAREN: End completions

  fn(h: .left, #^OVERLOADED_ARG2_LABEL^#)
// FIXME: Should only suggest 'v:' (rdar://problem/60346573).
//OVERLOADED_ARG2_LABEL: Begin completions, 2 items
//OVERLOADED_ARG2_LABEL-DAG: Pattern/ExprSpecific:               {#v: Functor.Vertical#}[#Functor.Vertical#];
//OVERLOADED_ARG2_LABEL-DAG: Pattern/ExprSpecific:               {#h: Functor.Horizontal#}[#Functor.Horizontal#];
//OVERLOADED_ARG2_LABEL: End completions

  fn(h: .left, v: .#^OVERLOADED_ARG2_VALUE^#)
// FIXME: Should only suggest 'up' and 'down' (rdar://problem/60346573).
//OVERLOADED_ARG2_VALUE: Begin completions, 6 items
//OVERLOADED_ARG2_VALUE-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: up[#Functor.Vertical#];
//OVERLOADED_ARG2_VALUE-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: down[#Functor.Vertical#];
//OVERLOADED_ARG2_VALUE-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): Functor.Vertical#})[#(into: inout Hasher) -> Void#];
//OVERLOADED_ARG2_VALUE-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: left[#Functor.Horizontal#];
//OVERLOADED_ARG2_VALUE-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: right[#Functor.Horizontal#];
//OVERLOADED_ARG2_VALUE-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): Functor.Horizontal#})[#(into: inout Hasher) -> Void#];
//OVERLOADED_ARG2_VALUE: End completions
}
