// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

private enum GlobalPrivateE {
    case foo, bar
}

func testGlobalPrivate(val: GlobalPrivateE) {
    val.#^GLOBALPRIVATE^#
// GLOBALPRIVATE: Begin completions, 3 items
// GLOBALPRIVATE-DAG: Keyword[self]/CurrNominal:          self[#GlobalPrivateE#];
// GLOBALPRIVATE-DAG: Decl[InstanceVar]/CurrNominal:      hashValue[#Int#];
// GLOBALPRIVATE-DAG: Decl[InstanceMethod]/CurrNominal:   hash({#into: &Hasher#})[#Void#];
// GLOBALPRIVATE: End completions
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
// LOCAL: End completions
}
