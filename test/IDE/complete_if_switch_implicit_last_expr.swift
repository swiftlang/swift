// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -enable-experimental-feature ImplicitLastExprResults -debug-forbid-typecheck-prefix FORBIDDEN -filecheck %raw-FileCheck -completion-output-dir %t

// Experimental feature requires asserts
// REQUIRES: asserts

enum E {
  case e
  case f(Int)
}

struct NO {
  // Triggering interface type computation of this will cause an assert to fire,
  // ensuring we don't attempt to type-check any references to it.
  static var TYPECHECK = FORBIDDEN
}

func test1() -> E {
  if .random() {
    ();
    .#^DOT1?check=DOT^#
  } else {
    .e
  }
}

func test2() -> E {
  switch Bool.random() {
  case true:
    .e
  case false:
    ();
    .#^DOT2?check=DOT^#
  }
}

func test3() throws -> E {
  switch Bool.random() {
  case true:
    NO.TYPECHECK
    throw NO.TYPECHECK
  case false:
    ();
    .#^DOT3?check=DOT^#
  }
}

struct S {
  var e: E
}

func test4() throws -> E {
  let x = switch Bool.random() {
  case true:
    NO.TYPECHECK
    throw NO.TYPECHECK
  case false:
    let s = S(e: .e)
    s.#^SDOT1?check=SDOT^#
  }
  return x
}

// SDOT:     Begin completions, 2 items
// SDOT-DAG: Keyword[self]/CurrNominal:          self[#S#]; name=self
// SDOT-DAG: Decl[InstanceVar]/CurrNominal:      e[#E#]; name=e

// DOT:     Begin completions, 2 items
// DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: e[#E#]; name=e
// DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: f({#Int#})[#E#]; name=f()
