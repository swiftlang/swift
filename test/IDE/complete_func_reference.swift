// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_0 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_1 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_2 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_3 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=VOID_VOID_4 | %FileCheck %s -check-prefix=VOID_VOID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_0 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_1 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_2 > %t.results
// RUN:%FileCheck %s -check-prefix=ANY_INT < %t.results
// RUN:%FileCheck %s -check-prefix=ANY_INT_STATIC_CURRY < %t.results
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_3 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_INT_4 | %FileCheck %s -check-prefix=ANY_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_0 | %FileCheck %s -check-prefix=INT_ANY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_1 | %FileCheck %s -check-prefix=INT_ANY
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_2 > %t.results
// RUN: %FileCheck %s -check-prefix=INT_ANY < %t.results
// RUN: %FileCheck %s -check-prefix=INT_ANY_STATIC_CURRY < %t.results
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INT_ANY_3 > %t.results
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
// VOID_VOID-DAG: Decl{{.*}}/TypeRelation[Convertible]: voidToVoid[#() -> ()#];
// VOID_VOID-DAG: Decl{{.*}}/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// VOID_VOID-DAG: Decl{{.*}}/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// VOID_VOID-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      anyToInt({#a: Any#})[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
// VOID_VOID-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// VOID_VOID-DAG: Decl{{.*}}:      returnsIntToInt()[#(Int) -> Int#];

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
do {
  func take(_: @escaping ((Any)->Int)???!) {}
  take(S0().#^ANY_INT_3^#)
}
do {
  let take: ((Any)->Int)?
  take = S0().#^ANY_INT_4^#
}

// ANY_INT-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToInt(a:)[#(Any) -> Int#]; name=anyToInt(a:)
// ANY_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// ANY_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// ANY_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: voidToVoid()[#Void#];
// ANY_INT-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
// ANY_INT-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// ANY_INT-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// ANY_INT-DAG: Decl{{.*}}:      returnsIntToInt()[#(Int) -> Int#];

// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: anyToInt({#(self): S0#})[#(a: Any) -> Int#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: voidToVoid({#(self): S0#})[#() -> Void#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: intToVoid({#(self): S0#})[#(a: Int) -> Void#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: anyToVoid({#(self): S0#})[#(a: Any) -> Void#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   voidToInt({#(self): S0#})[#() -> Int#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   intToInt({#(self): S0#})[#(a: Int) -> Int#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   voidToAny({#(self): S0#})[#() -> Any#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   anyToAny({#(self): S0#})[#(a: Any) -> Any#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   intToAny({#(self): S0#})[#(a: Int) -> Any#];
// ANY_INT_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   returnsIntToInt({#(self): S0#})[#() -> (Int) -> Int#];

do {
  func take(_: @escaping (Int)->Any) {}
  take(#^INT_ANY_0^#)
}

// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToAny(a:)[#(Int) -> Any#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToInt(a:)[#(Int) -> Int#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: intToVoid(a:)[#(Int) -> ()#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToAny(a:)[#(Any) -> Any#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToInt(a:)[#(Any) -> Int#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: anyToVoid(a:)[#(Any) -> ()#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Convertible]: returnsIntToInt()[#(Int) -> Int#];
// INT_ANY-DAG: Decl{{.*}}/TypeRelation[Invalid]: voidToVoid()[#Void#];
// INT_ANY-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// INT_ANY-DAG: Decl{{.*}}:      voidToAny()[#Any#];

// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: intToInt({#(self): S0#})[#(a: Int) -> Int#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: intToVoid({#(self): S0#})[#(a: Int) -> Void#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: anyToAny({#(self): S0#})[#(a: Any) -> Any#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: anyToVoid({#(self): S0#})[#(a: Any) -> Void#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: intToAny({#(self): S0#})[#(a: Int) -> Any#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: anyToInt({#(self): S0#})[#(a: Any) -> Int#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal: returnsIntToInt({#(self): S0#})[#() -> (Int) -> Int#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   voidToAny({#(self): S0#})[#() -> Any#];
// INT_ANY_STATIC_CURRY-DAG: Decl[InstanceMethod]/CurrNominal:   voidToInt({#(self): S0#})[#() -> Int#];

do {
  func take(_: @escaping (Int)->Any) {}
  take(S0().#^INT_ANY_1^#)
}
do {
  func take(_: @escaping (Int)->Any) {}
  take(S0.#^INT_ANY_2^#)
}
do {
  func take(_: @escaping ((Int)->Any)?) {}
  take(S0.#^INT_ANY_3^#)
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
// VOID_INT_INT-DAG: Decl{{.*}}/TypeRelation[Convertible]: returnsIntToInt[#() -> (Int) -> Int#];
// VOID_INT_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: intToVoid({#a: Int#})[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: anyToVoid({#a: Any#})[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}/TypeRelation[Invalid]: voidToVoid()[#Void#];
// VOID_INT_INT-DAG: Decl{{.*}}:      voidToAny()[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      intToAny({#a: Int#})[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      anyToAny({#a: Any#})[#Any#];
// VOID_INT_INT-DAG: Decl{{.*}}:      voidToInt()[#Int#];
// VOID_INT_INT-DAG: Decl{{.*}}:      anyToInt({#a: Any#})[#Int#];
// VOID_INT_INT-DAG: Decl{{.*}}:      intToInt({#a: Int#})[#Int#];
