// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_NODOT | %FileCheck %s -check-prefix=PERSONTYPE-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_DOT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARRAY_NODOT | %FileCheck %s -check-prefix=ARRAY-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARRAY_DOT | %FileCheck %s -check-prefix=ARRAY-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OBJ_NODOT | %FileCheck %s -check-prefix=OBJ-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OBJ_DOT | %FileCheck %s -check-prefix=OBJ-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_NODOT | %FileCheck %s -check-prefix=OPTIONAL-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONAL_DOT | %FileCheck %s -check-prefix=OPTIONAL-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNWRAPPED_NODOT | %FileCheck %s -check-prefix=OBJ-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNWRAPPED_DOT | %FileCheck %s -check-prefix=OBJ-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAIN_NODOT | %FileCheck %s -check-prefix=OBJ-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CHAIN_DOT | %FileCheck %s -check-prefix=OBJ-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARRAYTYPE_NODOT | %FileCheck %s -check-prefix=ARRAYTYPE-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARRAYTYPE_DOT | %FileCheck %s -check-prefix=ARRAYTYPE-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=APPLY_TYPE_DOT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=APPLY_OBJ_DOT | %FileCheck %s -check-prefix=OBJ-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EMPTY_1 | %FileCheck %s -check-prefix=INVALID
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EMPTY_2 | %FileCheck %s -check-prefix=INVALID

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_BASEONLY | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_EXPLICIT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_GENERIC_RESULT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_GENERIC_RESULT_OPTIONAL | %FileCheck %s -check-prefix=PERSONTYPE-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC_GENERICRESULT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC_ROOT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC_NONROOT | %FileCheck %s -check-prefix=OBJ-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC_INOUT | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_FUNC_VARIADIC | %FileCheck %s -check-prefix=ARRAYTYPE-DOT

// FIXME: We need the argument after the code completion token to determine the base of the key path in this test case. But since we ignore the types of arguments after the code completion token, we can't determine the base type.
// DISABLED: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_KEY_PATH_BASE | %FileCheck %s -check-prefix=PERSONTYPE-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_KEY_PATH_RESULT | %FileCheck %s -check-prefix=PERSONTYPE-DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE_AFTER_SELF | %FileCheck %s -check-prefix=OBJ-NODOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_RESULT_BUILDER | %FileCheck %s -check-prefix=PERSONTYPE-DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_MULTI_STMT_CLOSURE | %FileCheck %s -check-prefix=PERSONTYPE-DOT

class Person {
    var name: String
    var friends: [Person] = []
    var bestFriend: Person? = nil
    var itself: Person { return self }
    init(name: String) {
        self.name = name
    }
    func getName() -> String { return name }
    subscript(_ index: Int) -> Int { get { return 1} }
}

let _ = \Person#^TYPE_NODOT^#
// PERSONTYPE-NODOT: Begin completions, 5 items
// PERSONTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .name[#String#]; name=name
// PERSONTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .friends[#[Person]#]; name=friends
// PERSONTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .itself[#Person#]; name=itself
// PERSONTYPE-NODOT-DAG: Decl[Subscript]/CurrNominal:        .[{#(index): Int#}][#Int#]; name=[:]

let _ = \Person.#^TYPE_DOT^#
// PERSONTYPE-DOT: Begin completions, 5 items
// PERSONTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal:      name[#String#]; name=name
// PERSONTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal:      friends[#[Person]#]; name=friends
// PERSONTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal:      bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal:      itself[#Person#]; name=itself
// PERSONTYPE-DOT-DAG: Decl[Subscript]/CurrNominal:        [{#(index): Int#}][#Int#]; name=[:]

let _ = \Person.friends#^ARRAY_NODOT^#
// ARRAY-NODOT-DAG: Decl[Subscript]/CurrNominal/IsSystem:   [{#(index): Int#}][#Person#]; name=[:]
// ARRAY-NODOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: .count[#Int#]; name=count
// ARRAY-NODOT-DAG: Decl[InstanceVar]/Super/IsSystem:       .first[#Person?#]; name=first

let _ = \Person.friends.#^ARRAY_DOT^#
// ARRAY-DOT-NOT: Decl[Subscript]/CurrNominal/IsSystem:     [{#(index): Int#}][#Element#]; name=[]
// ARRAY-DOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem:   count[#Int#]; name=count
// ARRAY-DOT-DAG: Decl[InstanceVar]/Super/IsSystem:         first[#Person?#]; name=first
// ARRAY-DOT-NOT: Decl[Subscript]/CurrNominal/IsSystem:     [{#(index): Int#}][#Element#]; name=[]

let _ = \Person.friends[0]#^OBJ_NODOT^#
// OBJ-NODOT: Begin completions, 5 items
// OBJ-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .name[#String#]; name=name
// OBJ-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .friends[#[Person]#]; name=friends
// OBJ-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .bestFriend[#Person?#]; name=bestFriend
// OBJ-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .itself[#Person#]; name=itself
// OBJ-NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#(index): Int#}][#Int#]; name=[:]

let _ = \Person.friends[0].#^OBJ_DOT^#
// OBJ-DOT: Begin completions, 4 items
// OBJ-DOT-DAG: Decl[InstanceVar]/CurrNominal:      name[#String#]; name=name
// OBJ-DOT-DAG: Decl[InstanceVar]/CurrNominal:      friends[#[Person]#]; name=friends
// OBJ-DOT-DAG: Decl[InstanceVar]/CurrNominal:      bestFriend[#Person?#]; name=bestFriend
// OBJ-DOT-DAG: Decl[InstanceVar]/CurrNominal:      itself[#Person#]; name=itself

let _ = \Person.bestFriend#^OPTIONAL_NODOT^#
// OPTIONAL-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      ?.name[#String#]; name=name
// OPTIONAL-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      ?.friends[#[Person]#]; name=friends
// OPTIONAL-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      ?.bestFriend[#Person?#]; name=bestFriend
// OPTIONAL-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      ?.itself[#Person#]; name=itself
// OPTIONAL-NODOT-DAG: Decl[Subscript]/CurrNominal:        ?[{#(index): Int#}][#Int#]; name=[:]
// OPTIONAL-NODOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem:  .unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped

let _ = \Person.bestFriend.#^OPTIONAL_DOT^#
// OPTIONAL-DOT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.name[#String#]; name=name
// OPTIONAL-DOT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.friends[#[Person]#]; name=friends
// OPTIONAL-DOT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.bestFriend[#Person?#]; name=bestFriend
// OPTIONAL-DOT-DAG: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.itself[#Person#]; name=itself
// OPTIONAL-DOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem:      unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped

let _ = \Person.bestFriend?#^UNWRAPPED_NODOT^#
// Same as OBJ_NODOT.

let _ = \Person.bestFriend?.#^UNWRAPPED_DOT^#
// Same as OBJ_DOT.

let _ = \Person.bestFriend?.itself#^CHAIN_NODOT^#
// Same as OBJ_NODOT.

let _ = \Person.bestFriend?.itself.#^CHAIN_DOT^#
// Same as OBJ_DOT.

let _ = \[Person]#^ARRAYTYPE_NODOT^#
// ARRAYTYPE-NODOT-DAG: Decl[Subscript]/CurrNominal/IsSystem:   .[{#(index): Int#}][#Person#]; name=[:]
// ARRAYTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: .count[#Int#]; name=count
// ARRAYTYPE-NODOT-DAG: Decl[InstanceVar]/Super/IsSystem:       .first[#Person?#]; name=first

let _ = \[Person].#^ARRAYTYPE_DOT^#
// ARRAYTYPE-DOT-DAG: Decl[Subscript]/CurrNominal/IsSystem:   [{#(index): Int#}][#Person#]; name=[:]
// ARRAYTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal/IsSystem: count[#Int#]; name=count
// ARRAYTYPE-DOT-DAG: Decl[InstanceVar]/Super/IsSystem:       first[#Person?#]; name=first

func test(_ p: Person) {
  let _ = p[keyPath: \Person.#^APPLY_TYPE_DOT^#]
  // Same as TYPE_DOT.
  let _ = p[keyPath: \Person.friends[0].#^APPLY_OBJ_DOT^#]
  // Same as OBJ_DOT.
}

let _ = \.#^EMPTY_1^#
let _ = \.friends.#^EMPTY_2^#
// INVALID-NOT: Begin completions

func recvPartialKP(_ kp: PartialKeyPath<Person>) {
  recvPartialKP(\.#^CONTEXT_BASEONLY^#)
  // Same as TYPE_DOT.
}
func recvExplicitKP(_ kp: KeyPath<Person, String>) {
  recvExplicitKP(\.#^CONTEXT_EXPLICIT^#)
  // Same as TYPE_DOT.
}
func recvExplicitKPWithGenericResult<Result>(_ kp: KeyPath<Person, Result>) {
  recvExplicitKPWithGenericResult(\.#^CONTEXT_GENERIC_RESULT^#)
  // Same as TYPE_DOT.
}
func recvExplicitKPWithGenericResultOpt<Result>(_ kp: KeyPath<Person, Result>?) {
  recvExplicitKPWithGenericResult(\.#^CONTEXT_GENERIC_RESULT_OPTIONAL^#
  // Same as TYPE_DOT.
}
func recvFunc(_ fn: (Person) -> String) {
  recvFunc(\.#^CONTEXT_FUNC^#)
}
func recvFuncGeneric<T>(_ fn: (Person) -> T) {
  recvFunc(\.#^CONTEXT_FUNC_GENERICRESULT^#)
}

struct Wrap<T> {
  func map<U>(_ fn: (T) -> U) -> U { fatalError() }
  func _inout<U>(_ fn: (inout T) -> U) -> U { fatalError() }
  func variadic<U>(_ fn: (T...) -> U) -> U { fatalError() }
}
func testKeyPathAsFunctions(wrapped: Wrap<Person>) {
  let _ = wrapped.map(\.#^CONTEXT_FUNC_ROOT^#)
  // Same as TYPE_DOT.
  let _ = wrapped.map(\.friends[0].#^CONTEXT_FUNC_NONROOT^#)
  // Same as OBJ_DOT.
  let _ = wrapped._inout(\.#^CONTEXT_FUNC_INOUT^#)
  // Same as TYPE_DOT.
  let _ = wrapped.variadic(\.#^CONTEXT_FUNC_VARIADIC^#)
  // Same as ARRAYTYPE_DOT.
}

func genericKeyPathBase<Root>(to keyPath: ReferenceWritableKeyPath<Root, Person>, on object: Root) {
  genericKeyPathBase(to: \.#^GENERIC_KEY_PATH_BASE^#, on: Person())
  // Same as TYPE_DOT.
}

func genericKeyPathResult<KeyPathResult>(id: KeyPath<Person, KeyPathResult>) {
  genericKeyPathResult(\.#^GENERIC_KEY_PATH_RESULT^#)
  // Same as TYPE_DOT.
}

func completeAfterSelf(people: [Person]) {
  people.map(\.self#^COMPLETE_AFTER_SELF^#)
}

func inResultBuilder() {
  protocol View2 {}

  @resultBuilder public struct ViewBuilder2 {
    public static func buildBlock<Content>(_ content: Content) -> Content where Content : View2 { fatalError() }
    public static func buildIf<Content>(_ content: Content?) -> Content? where Content : View2 { fatalError() }
  }

  struct VStack2<Content>: View2 {
    init(@ViewBuilder2 view: () -> Content) {}
  }

  @ViewBuilder2 var body: some View2 {
    VStack2 {
      if true {
        var people: [Person] = []
        people.map(\.#^IN_RESULT_BUILDER^#)
      }
    }
  }
}

func inMultiStmtClosure(closure: () -> Void) {
  inMultiStmtClosure {
    var people: [Person] = []
    people.map(\.#^IN_MULTI_STMT_CLOSURE^#)
  }
}
