// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_0 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_1 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_2 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_3 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_4 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_0 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_1 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_2 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_0 | %FileCheck %s -check-prefix=INT_ANY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_1 | %FileCheck %s -check-prefix=INT_ANY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_2 | %FileCheck %s -check-prefix=INT_ANY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_INT_INT_0 | %FileCheck %s -check-prefix=VOID_INT_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_INT_INT_1 | %FileCheck %s -check-prefix=VOID_INT_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_INT_INT_2 | %FileCheck %s -check-prefix=VOID_INT_INT

func voidToVoid() {}
func voidToInt() -> Int {}
func intToInt(a: Int) -> Int {}
func intToVoid(a: Int) {}

func voidToAny() -> Any {}
func anyToAny(a: Any) -> Any {}
func anyToVoid(a: Any) {}

func intToAny(a: Int) -> Any {}
func anyToInt(a: Any) -> Int {}

func returnsIntToInt() -> (Int) -> Int {}

struct S0 {
  func voidToVoid() {}
  func voidToInt() -> Int {}
  func intToInt(a: Int) -> Int {}
  func intToVoid(a: Int) {}

  func voidToAny() -> Any {}
  func anyToAny(a: Any) -> Any {}
  func anyToVoid(a: Any) {}

  func intToAny(a: Int) -> Any {}
  func anyToInt(a: Any) -> Int {}

  func returnsIntToInt() -> (Int) -> Int {}

  static func voidToVoid() {}
  static func voidToInt() -> Int {}
  static func intToInt(a: Int) -> Int {}
  static func intToVoid(a: Int) {}

  static func voidToAny() -> Any {}
  static func anyToAny(a: Any) -> Any {}
  static func anyToVoid(a: Any) {}

  static func intToAny(a: Int) -> Any {}
  static func anyToInt(a: Any) -> Int {}

  static func returnsIntToInt() -> (Int) -> Int {}
}

do {
  func take(_: @escaping ()->()) {}
  take(#^VOID_VOID_0^#)
}
// VOID_VOID: Begin completions
// VOID_VOID-DAG: Decl{{.*}}/TypeRelation[Identical]: voidToVoid;
// VOID_VOID-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToVoid(a:);
// VOID_VOID-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// VOID_VOID-DAT: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// VOID_VOID-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      anyToInt({#a: Any#})[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      returnsIntToInt()[#(Int) -> Int#];
// VOID_VOID: End completions

do {
  func take(_: Int, _: Int, c: @escaping ()->()) {}
  take(1, 2, c: #^VOID_VOID_1^#)
}

do {
  let take: ()->()
  take = #^VOID_VOID_2^#
}
do {
  let take: ()->()
  take = S0().#^VOID_VOID_3^#
}
do {
  let take: ()->()
  take = S0.#^VOID_VOID_4^#
}

do {
  func take(_: @escaping (Any)->Int) {}
  take(#^ANY_INT_0^#)
}
do {
  func take(_: @escaping (Any)->Int) {}
  take(S0().#^ANY_INT_1^#)
}
do {
  func take(_: @escaping (Any)->Int) {}
  take(S0.#^ANY_INT_2^#)
}

// ANY_INT: Begin completions
// ANY_INT-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToInt(a:);
// ANY_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// ANY_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// ANY_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: voidToVoid()[#Void#];
// ANY_INT-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
// ANY_INT-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// ANY_INT-DAG: Decl{{.*}}:      returnsIntToInt()[#(Int) -> Int#];
// ANY_INT: End completions

do {
  func take(_: @escaping (Int)->Any) {}
  take(#^INT_ANY_0^#)
}

// INT_ANY: Begin completions
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToAny(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToInt(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToVoid(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToAny(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToInt(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToVoid(a:);
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: returnsIntToInt()[#(Int) -> Int#];
// INT_ANY-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: voidToVoid()[#Void#];
// INT_ANY-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// INT_ANY-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// INT_ANY: End completions

do {
  func take(_: @escaping (Int)->Any) {}
  take(S0().#^INT_ANY_1^#)
}
do {
  func take(_: @escaping (Int)->Any) {}
  take(S0.#^INT_ANY_2^#)
}

do {
  func take(_: @escaping ()->(Int)->Int) {}
  take(#^VOID_INT_INT_0^#)
}
do {
  func take(_: @escaping ()->(Int)->Int) {}
  take(S0().#^VOID_INT_INT_1^#)
}
do {
  func take(_: @escaping ()->(Int)->Int) {}
  take(S0.#^VOID_INT_INT_2^#)
}
// VOID_INT_INT-DAG: Decl{{.*}}/TypeRelation[Identical]: returnsIntToInt;
// VOID_INT_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}/NotRecommended/TypeRelation[Invalid]: voidToVoid()[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// VOID_INT_INT-DAG: Decl{{.*}}:      anyToInt({#a: Any#})[#Int#];
// VOID_INT_INT-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
