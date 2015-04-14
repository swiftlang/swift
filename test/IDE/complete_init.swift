// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=TOP_LEVEL_0 | FileCheck %s -check-prefix=TOP_LEVEL_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=TOP_LEVEL_1 > %t.generic
// RUN: FileCheck %s -check-prefix=TOP_LEVEL_0 < %t.generic
// RUN: FileCheck %s -check-prefix=GENERIC_PARAM_0 < %t.generic
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=K_QUALIFIED_0 | FileCheck %s -check-prefix=K_QUALIFIED_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=L_QUALIFIED_0 | FileCheck %s -check-prefix=L_QUALIFIED_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=INSIDE_L_0 | FileCheck %s -check-prefix=INSIDE_L_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-complete-inits-in-postfix-expr -code-completion-token=INSIDE_M_0 | FileCheck %s -check-prefix=INSIDE_M_0

struct A {
  // implicit init()
}

struct B {
  let x: A, y: A, z: A
  // implicit init(x:,y:,z:)
}

struct C {
  init() {}
  init(x: A) {}
  init(y: A=A()) {}
}

class D {
  // implicit init()
}
class E {
  init(x: A) {}
}

class F : E {
  // inherited init(x: A)
  convenience init() {}
}

protocol G {
  init(x: A)
}

struct H : G {
  init(x: A) {}
}

protocol I : G {
  init(y: A)
}

typealias J = A

struct  K {
  typealias X = A
  struct Y {}
}

struct L<X: G> {
  typealias Y = X
}

// NO_ERRORS_UP_TO_HERE


func testTopLevel() {
  #^TOP_LEVEL_0^#
}
// TOP_LEVEL_0: Begin completions
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    A()[#A#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    B({#x: A#}, {#y: A#}, {#z: A#})[#B#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    C()[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    C({#x: A#})[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    C({#y: A#})[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    D()[#D#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    E({#x: A#})[#E#]{{; name=.+}}
// TOP_LEVEL_0-DAG-NOT: Decl[Constructor]/CurrModule:    E()
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    F({#x: A#})[#F#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    F()[#F#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    G({#x: A#})[#Self#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    H({#x: A#})[#H#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    I({#x: A#})[#Self#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    I({#y: A#})[#Self#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/CurrModule:    J()[#A#]{{; name=.+}}
// TOP_LEVEL_0: End completions

func testQualified0() {
  K.#^K_QUALIFIED_0^#
}
// K_QUALIFIED_0: Begin completions
// K_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    X()[#A#]{{; name=.+}}
// K_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    Y()[#K.Y#]{{; name=.+}}
// K_QUALIFIED_0: End completions

func testQualified1() {
  L.#^L_QUALIFIED_0^#
}
// L_QUALIFIED_0: Begin completions
// L_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#Self#]{{; name=.+}}
// L_QUALIFIED_0-DAG-NOT: X({#x: A#})
// L_QUALIFIED_0: End completions

func testGenericParam<T: I, U: G>() {
  #^TOP_LEVEL_1^#
}
// GENERIC_PARAM_0: Begin completions
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    T({#x: A#})[#Self#]{{; name=.+}}
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    T({#y: A#})[#Self#]{{; name=.+}}
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    U({#x: A#})[#Self#]{{; name=.+}}
// GENERIC_PARAM_0-DAG-NOT: Decl[Constructor]/Local:    U({#y
// GENERIC_PARAM_0: End completions

extension L {
  func test() {
    #^INSIDE_L_0^#
  }
}
// INSIDE_L_0: Begin completions
// INSIDE_L_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#Self#]{{; name=.+}}

// FIXME: <rdar://problem/20530021> Code complete generic parameters in extensions
// disabled_INSIDE_L_0-DAG: Decl[Constructor]/CurrNominal:    X({#x: A#})[#Self#]{{; name=.+}}

// INSIDE_L_0: End completions

struct M<X: G> {
  typealias Y = X
  func test() {
    #^INSIDE_M_0^#
  }
}
// INSIDE_M_0: Begin completions
// INSIDE_M_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#Self#]{{; name=.+}}
// INSIDE_M_0-DAG: Decl[Constructor]/CurrNominal:    X({#x: A#})[#Self#]{{; name=.+}}
// INSIDE_M_0: End completions
