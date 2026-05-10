// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_INFER_DOT_OPTIONAL | %FileCheck %s -check-prefix=PERSONTYPE-INFER-DOT-OPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_DOT_OPTIONAL | %FileCheck %s -check-prefix=PERSONTYPE-DOT-OPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_DOT_OPTIONAL_SPACE | %FileCheck %s -check-prefix=PERSONTYPE-DOT-OPT-SPACE

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

extension Optional {
    var optMember: String { "member" }
}

let _ : KeyPath<Person?, String> = \.#^TYPE_INFER_DOT_OPTIONAL^#
// PERSONTYPE-INFER-DOT-OPT: Begin completions, 9 items
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.name[#String#]; name=name
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.friends[#[Person]#]; name=friends
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.itself[#Person#]; name=itself
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[Subscript]/CurrNominal:     ?[{#(index): Int#}][#Int#]; name=[:]
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:            optMember[#String#]; name=optMember
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   debugDescription[#String#]; name=debugDescription
// PERSONTYPE-INFER-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   customMirror[#Mirror#]; name=customMirror

let _ : KeyPath<Person?, String> = \Person?.#^TYPE_DOT_OPTIONAL^#
// PERSONTYPE-DOT-OPT: Begin completions, 9 items
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.name[#String#]; name=name
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.friends[#[Person]#]; name=friends
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:   ?.itself[#Person#]; name=itself
// PERSONTYPE-DOT-OPT-NEXT: Decl[Subscript]/CurrNominal:     ?[{#(index): Int#}][#Int#]; name=[:]
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal:            optMember[#String#]; name=optMember
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   debugDescription[#String#]; name=debugDescription
// PERSONTYPE-DOT-OPT-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   customMirror[#Mirror#]; name=customMirror

let _ : KeyPath<Person?, String> = \Person?. #^TYPE_DOT_OPTIONAL_SPACE^#
// PERSONTYPE-DOT-OPT-SPACE: Begin completions, 9 items
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]:   ?.name[#String#]; name=name
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]:   ?.friends[#[Person]#]; name=friends
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]:   ?.bestFriend[#Person?#]; name=bestFriend
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/Erase[1]:   ?.itself[#Person#]; name=itself
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[Subscript]/CurrNominal/Erase[1]:     ?[{#(index): Int#}][#Int#]; name=[:]
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal:            optMember[#String#]; name=optMember
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   unsafelyUnwrapped[#Person#]; name=unsafelyUnwrapped
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   debugDescription[#String#]; name=debugDescription
// PERSONTYPE-DOT-OPT-SPACE-NEXT: Decl[InstanceVar]/CurrNominal/IsSystem:   customMirror[#Mirror#]; name=customMirror
