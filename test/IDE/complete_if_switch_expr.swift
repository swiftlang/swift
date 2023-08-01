// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

enum E {
  case e
  case f(Int)
}

func ifExprDotReturn() -> E {
  if .random() {
    .#^DOT1?check=DOT^#
  } else {
    .e
  }
}

func switchExprDotReturn() -> E {
  switch Bool.random() {
  case true:
    .e
  case false:
    .#^DOT2?check=DOT^#
  }
}

func ifExprDotClosureReturn() -> E {
  let x: E = {
    if .random() {
      .e
    } else {
      .#^DOT3?check=DOT^#
    }
  }()
  return x
}

func switchExprDotClosureReturn() -> E {
  let x: E = {
    switch Bool.random() {
    case true:
      .#^DOT4?check=DOT^#
    case false:
      .e
    }
  }()
  return x
}

func ifExprBranchInferenceReturn1() -> E {
  let fn = {
    if .random() {
      E.e
    } else {
      .#^DOT5?check=DOT^#
    }
  }
  return fn()
}

func switchExprBranchInferenceReturn1() -> E {
  let fn = {
    switch Bool.random() {
    case true:
      E.e
    case false:
      .#^DOT6?check=DOT^#
    }
  }
  return fn()
}

func ifExprBranchInferenceReturn2() -> E {
  let fn = {
    if .random() {
      .#^DOT7?check=DOT^#
    } else {
      E.e
    }
  }
  return fn()
}

func switchExprBranchInferenceReturn2() -> E {
  let fn = {
    switch Bool.random() {
    case true:
      .#^DOT8?check=DOT^#
    case false:
      E.e
    }
  }
  return fn()
}

func ifExprBinding() -> E {
  let x: E = 
    if .random() {
      .e
    } else {
      .#^DOT9?check=DOT^#
    }
  return x
}

func switchExprBinding() -> E {
  let x: E =
    switch Bool.random() {
      case true:
        .e
      case false:
        .#^DOT10?check=DOT^#
    }
  return x
}

// DOT:     Begin completions, 2 items
// DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: e[#E#]; name=e
// DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: f({#Int#})[#E#]; name=f()
