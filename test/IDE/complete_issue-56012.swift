// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE | %FileCheck %s --check-prefix=OVERRIDE
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=MEMBER | %FileCheck %s --check-prefix=MEMBER

// https://github.com/apple/swift/issues/56012

class Root {
    func onRoot() {}
}

class Base<T: Hashable>: Root {
    func onBase() -> T {}
}

class Derived<T: Hashable>: Base<T> {
    func onDerived() {}

    func #^OVERRIDE^#
// OVERRIDE: Begin completions, 2 items
// OVERRIDE-DAG: Decl[InstanceMethod]/Super/Erase[5]: override func onBase() -> T {|};
// OVERRIDE-DAG: Decl[InstanceMethod]/Super/Erase[5]: override func onRoot() {|};

}

func testMember(val: Derived<Int>) {
    val.#^MEMBER^#
// MEMBER: Begin completions, 4 items
// MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Derived<Int>#]; name=self
// MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   onDerived()[#Void#]; name=onDerived()
// MEMBER-DAG: Decl[InstanceMethod]/Super:         onBase()[#Int#]; name=onBase()
// MEMBER-DAG: Decl[InstanceMethod]/Super:         onRoot()[#Void#]; name=onRoot()
}
