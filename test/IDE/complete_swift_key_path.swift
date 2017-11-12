// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_1 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_2 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_3 | %FileCheck %s -check-prefix=PERSON-MEMBER

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_5 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_6 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_7 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_8 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_9 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_10 | %FileCheck %s -check-prefix=PERSON-MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PART_11 | %FileCheck %s -check-prefix=PERSON-MEMBER

class Person {
    var name: String
    var friends: [Person] = []
    var bestFriend: Person? = nil
    init(name: String) {
        self.name = name
    }
    func getName() -> String { return name }
    subscript(_ index: Int) -> Int { get { return 1} }
}

let keyPath1 = \Person.#^PART_1^#
let keyPath2 = \Person.friends[0].#^PART_2^#
let keyPath3 = \Person.friends[0].friends[0].friends[0].#^PART_3^#

// FIXME: the optionality keypath should work after our compiler is ready.
let keyPath4 = \Person.bestFriend?.#^PART_4^#
let keyPath5 = \Person.friends.[0].friends[0].friends[0].#^PART_5^#
let keyPath6 = \[Person].[0].#^PART_6^#
let keyPath7 = \[Person].[0].friends[0].#^PART_7^#

func foo1(_ p : Person) {
  _ = p[keyPath:\Person.#^PART_8^#]
  _ = p[keyPath:\Person.friends[0].#^PART_9^#]
  _ = p[keyPath:\[Person].[0].#^PART_10^#]
  _ = p[keyPath:\Person.friends.[0].friends[0].friends[0].#^PART_11^#]
}

// PERSON-MEMBER: Begin completions, 4 items
// PERSON-MEMBER-NEXT: Decl[InstanceVar]/CurrNominal:      name[#String#]; name=name
// PERSON-MEMBER-NEXT: Decl[InstanceVar]/CurrNominal:      friends[#[Person]#]; name=friends
// PERSON-MEMBER-NEXT: Decl[InstanceVar]/CurrNominal:      bestFriend[#Person?#]; name=bestFriend 
// PERSON-MEMBER-NEXT: Decl[Subscript]/CurrNominal:        [{#Int#}][#Int#]; name=[Int]
