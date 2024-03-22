// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)
// RUN: %empty-directory(%t/co-same)
// RUN: %empty-directory(%t/co-across)
// RUN: split-file %s %t --leading-lines

// Completion in the current file/module
// RUN: %target-swift-ide-test -batch-code-completion -code-complete-inits-in-postfix-expr -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t/co-same

// Completion across modules
// RUN: %target-swift-frontend -emit-module %t/InitTypes.swift -o %t/mods/
// RUN: %target-swift-ide-test -batch-code-completion -code-complete-inits-in-postfix-expr -source-filename %t/InitUses.swift -filecheck %raw-FileCheck -completion-output-dir %t/co-across -I %t/mods -D SEPARATED

//--- InitTypes.swift
public struct A {
  public init() {}
}

public struct B {
  let x: A, y: A, z: A
  public init(x: A, y: A, z: A) {
    self.x = x
    self.y = y
    self.z = z
  }
}

public struct C {
  public init() {}
  public init(x: A) {}
  public init(y: A=A()) {}
}

public class D {
  public init() {}
}
public class E {
  public init(x: A) {}
}

public class F : E {
  // inherited init(x: A)
  public convenience init() {
    self.init(x: A())
  }
}

public protocol G {
  init(x: A)
}

public struct H : G {
  public init(x: A) {}
}

public protocol I : G {
  init(y: A)
}

public typealias J = A

public struct K {
  public typealias X = A
  public struct Y {
    public init() {}
  }
}

public struct L<X: G> {
  public typealias Y = X
}

//--- InitUses.swift
#if SEPARATED
import InitTypes
#endif

func testTopLevel() {
  #^TOP_LEVEL_0?check=TOP_LEVEL_0;check=NEGATIVE_TOP_LEVEL_0^#
}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    A()[#A#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    B({#x: A#}, {#y: A#}, {#z: A#})[#B#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    C()[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    C({#x: A#})[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    C({#y: A#})[#C#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    D()[#D#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    E({#x: A#})[#E#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    F({#x: A#})[#F#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    F()[#F#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    H({#x: A#})[#H#]{{; name=.+}}
// TOP_LEVEL_0-DAG: Decl[Constructor]/{{.*}}:    J()[#A#]{{; name=.+}}
// NEGATIVE_TOP_LEVEL_0-NOT: Decl[Constructor]/{{.*}}:    E()
// NEGATIVE_TOP_LEVEL_0-NOT: Decl[Constructor]/{{.*}}:    G(
// NEGATIVE_TOP_LEVEL_0-NOT: Decl[Constructor]/{{.*}}:    I(

func testQualified0() {
  K.#^K_QUALIFIED_0^#
}
// K_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    X()[#A#]{{; name=.+}}
// K_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    Y()[#K.Y#]{{; name=.+}}

func testQualified1() {
  L.#^L_QUALIFIED_0^#
}
// L_QUALIFIED_0-NOT: X({#x: A#})
// L_QUALIFIED_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#G#]{{; name=.+}}

func testGenericParam<T: I, U: G>() {
  #^TOP_LEVEL_1?check=TOP_LEVEL_0;check=GENERIC_PARAM_0^#
}
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    T({#x: A#})[#I#]{{; name=.+}}
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    T({#y: A#})[#I#]{{; name=.+}}
// GENERIC_PARAM_0-DAG: Decl[Constructor]/Local:    U({#x: A#})[#G#]{{; name=.+}}
// GENERIC_PARAM_0-NOT: Decl[Constructor]/Local:    U({#y

extension L {
  func test() {
    #^INSIDE_L_0^#
  }
}
// INSIDE_L_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#G#]{{; name=.+}}

// FIXME: <rdar://problem/20530021> Code complete generic parameters in extensions
// INSIDE_L_0-DAG: Decl[GenericTypeParam]/Local:     X[#X#]; name=X
// INSIDE_L_0-DAG: Decl[Constructor]/Local:          X({#x: A#})[#G#]{{; name=.+}}


struct M<X: G> {
  typealias Y = X
  func test() {
    #^INSIDE_M_0^#
  }
}
// INSIDE_M_0-DAG: Decl[Constructor]/CurrNominal:    Y({#x: A#})[#G#]{{; name=.+}}
// INSIDE_M_0-DAG: Decl[Constructor]/Local:          X({#x: A#})[#G#]{{; name=.+}}

typealias CAlias = C

var CAliasInstance = CAlias(#^ALIAS_CONSTRUCTOR_0^#
// rdar://18586415
// ALIAS_CONSTRUCTOR_0: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#x: A#}[')'][#CAlias#];
// ALIAS_CONSTRUCTOR_0: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#y: A#}[')'][#CAlias#];

// https://github.com/apple/swift/issues/57916
struct Issue57916 {
  var a: Int
  var b: Int {
    get { return a }
  }
}
func test57916() {
  Issue57916(#^ISSUE_57916^#
  // ISSUE_57916: Decl[Constructor]/CurrNominal/Flair[ArgLabels]: ['(']{#a: Int#}[')'][#Issue57916#]; name=a:
}
