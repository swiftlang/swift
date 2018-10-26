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
// FIXME: RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONTEXT_EXPLICIT | %FileCheck %s -check-prefix=INVALID

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
// PERSONTYPE-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .name[#String#]; name=name
// PERSONTYPE-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .friends[#[Person]#]; name=friends
// PERSONTYPE-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .itself[#Person#]; name=itself
// PERSONTYPE-NODOT-NEXT: Decl[Subscript]/CurrNominal:        .[{#Int#}][#Int#]; name=[Int]

let _ = \Person.#^TYPE_DOT^#
// PERSONTYPE-DOT: Begin completions, 5 items
// PERSONTYPE-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      name[#String#]; name=name
// PERSONTYPE-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      friends[#[Person]#]; name=friends
// PERSONTYPE-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      itself[#Person#]; name=itself
// PERSONTYPE-DOT-NEXT: Decl[Subscript]/CurrNominal:        [{#Int#}][#Int#]; name=[Int]

let _ = \Person.friends#^ARRAY_NODOT^#
// ARRAY-NODOT: Begin completions
// ARRAY-NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#Int#}][#Element#]; name=[Int]
// ARRAY-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .count[#Int#]; name=count
// ARRAY-NODOT-DAG: Decl[InstanceVar]/Super:            .first[#Person?#]; name=first

let _ = \Person.friends.#^ARRAY_DOT^#
// ARRAY-DOT: Begin completions
// ARRAY-DOT-NOT: Decl[Subscript]/CurrNominal:        [{#Int#}][#Element#]; name=[Int]
// ARRAY-DOT-DAG: Decl[InstanceVar]/CurrNominal:      count[#Int#]; name=count
// ARRAY-DOT-DAG: Decl[InstanceVar]/Super:            first[#Person?#]; name=first
// ARRAY-DOT-NOT: Decl[Subscript]/CurrNominal:        [{#Int#}][#Element#]; name=[Int]

let _ = \Person.friends[0]#^OBJ_NODOT^#
// OBJ-NODOT: Begin completions, 5 items
// OBJ-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .name[#String#]; name=name
// OBJ-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .friends[#[Person]#]; name=friends
// OBJ-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .bestFriend[#Person?#]; name=bestFriend
// OBJ-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      .itself[#Person#]; name=itself
// OBJ-NODOT-NEXT: Decl[Subscript]/CurrNominal:        [{#Int#}][#Int#]; name=[Int]

let _ = \Person.friends[0].#^OBJ_DOT^#
// OBJ-DOT: Begin completions, 5 items
// OBJ-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      name[#String#]; name=name
// OBJ-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      friends[#[Person]#]; name=friends
// OBJ-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      bestFriend[#Person?#]; name=bestFriend
// OBJ-DOT-NEXT: Decl[InstanceVar]/CurrNominal:      itself[#Person#]; name=itself
// OBJ-DOT-NEXT: Decl[Subscript]/CurrNominal/Erase[1]: [{#Int#}][#Int#]; name=[Int]

let _ = \Person.bestFriend#^OPTIONAL_NODOT^#
// OPTIONAL-NODOT: Begin completions
// OPTIONAL-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      ?.name[#String#]; name=name
// OPTIONAL-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      ?.friends[#[Person]#]; name=friends
// OPTIONAL-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      ?.bestFriend[#Person?#]; name=bestFriend
// OPTIONAL-NODOT-NEXT: Decl[InstanceVar]/CurrNominal:      ?.itself[#Person#]; name=itself
// OPTIONAL-NODOT-NEXT: Decl[Subscript]/CurrNominal:        ?[{#Int#}][#Int#]; name=[Int]
// OPTIONAL-NODOT: Decl[InstanceVar]/CurrNominal:      .unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped

let _ = \Person.bestFriend.#^OPTIONAL_DOT^#
// OPTIONAL-DOT: Begin completions
// OPTIONAL-DOT-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.name[#String#]; name=name
// OPTIONAL-DOT-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.friends[#[Person]#]; name=friends
// OPTIONAL-DOT-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.bestFriend[#Person?#]; name=bestFriend
// OPTIONAL-DOT-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]: ?.itself[#Person#]; name=itself
// OPTIONAL-DOT-NEXT: Decl[Subscript]/CurrNominal/Erase[1]: ?[{#Int#}][#Int#]; name=[Int]
// OPTIONAL-DOT: Decl[InstanceVar]/CurrNominal:      unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped

let _ = \Person.bestFriend?#^UNWRAPPED_NODOT^#
// Same as OBJ_NODOT.

let _ = \Person.bestFriend?.#^UNWRAPPED_DOT^#
// Same as OBJ_DOT.

let _ = \Person.bestFriend?.itself#^CHAIN_NODOT^#
// Same as OBJ_NODOT.

let _ = \Person.bestFriend?.itself.#^CHAIN_DOT^#
// Same as OBJ_DOT.

let _ = \[Person]#^ARRAYTYPE_NODOT^#
// ARRAYTYPE-NODOT: Begin completions
// ARRAYTYPE-NODOT-DAG: Decl[Subscript]/CurrNominal:        .[{#Int#}][#Element#]; name=[Int]
// ARRAYTYPE-NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .count[#Int#]; name=count
// ARRAYTYPE-NODOT-DAG: Decl[InstanceVar]/Super:            .first[#Person?#]; name=first

let _ = \[Person].#^ARRAYTYPE_DOT^#
// ARRAYTYPE-DOT: Begin completions
// ARRAYTYPE-DOT-DAG: Decl[Subscript]/CurrNominal:        [{#Int#}][#Element#]; name=[Int]
// ARRAYTYPE-DOT-DAG: Decl[InstanceVar]/CurrNominal:      count[#Int#]; name=count
// ARRAYTYPE-DOT-DAG: Decl[InstanceVar]/Super:            first[#Person?#]; name=first

func test(_ p: Person) {
  let _ = p[keyPath: \Person.#^APPLY_TYPE_DOT^#]
  // Same as TYPE_DOT.
  let _ = p[keyPath: \Person.friends[0].#^APPLY_OBJ_DOT^#]
  // Same as OBJ_DOT.
}

let _ = \.#^EMPTY_1^#
let _ = \.friends.#^EMPTY_2^#
// INVALID-NOT: Begin completions

let _: PartialKeyPath<Person> = \.#^CONTEXT_BASEONLY^#
// Same as TYPE_DOT.

let _: KeyPath<Person, String> = \.#^CONTEXT_EXPLICIT^#
// FIXME: Currently, it's suggest nothing. Since we know the base type is
// 'Person', we should suggest at least '.name'.
