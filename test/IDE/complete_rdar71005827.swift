// RUN: %batch-code-completion

private enum GlobalPrivateE {
    case foo, bar
}

func testGlobalPrivate(val: GlobalPrivateE) {
    val.#^GLOBALPRIVATE^#
// GLOBALPRIVATE: Begin completions, 3 items
// GLOBALPRIVATE-DAG: Keyword[self]/CurrNominal:          self[#GlobalPrivateE#];
// GLOBALPRIVATE-DAG: Decl[InstanceVar]/CurrNominal:      hashValue[#Int#];
// GLOBALPRIVATE-DAG: Decl[InstanceMethod]/CurrNominal:   hash({#into: &Hasher#})[#Void#];
}

func testLocal() {
    enum LocalE {
        case foo, bar
    }
    var val = LocalE.foo
    val.#^LOCAL^#
// LOCAL: Begin completions, 3 items
// LOCAL-DAG: Keyword[self]/CurrNominal:          self[#LocalE#];
// LOCAL-DAG: Decl[InstanceVar]/CurrNominal:      hashValue[#Int#];
// LOCAL-DAG: Decl[InstanceMethod]/CurrNominal:   hash({#into: &Hasher#})[#Void#];
}
