// REQUIRES: concurrency
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -enable-experimental-concurrency

class MyNonSendable {}
struct MySendable {}

actor MyActor {
  var property: Int
  func syncFunc() -> Int { 1 }
  func syncNonSendable(arg: MyNonSendable) -> Int { 1 }
  func syncSendable(arg: MySendable) -> Int { 1 }
  func asyncFunc() async -> Int { 1 }
  subscript(idx: Int) -> Int { get { 1 } set { } }
}

extension MyActor {
    func testSyncFunc(other: MyActor) {
        let _ = #^IN_SYNCFUNC^#
// IN_SYNCFUNC: Begin completions
// IN_SYNCFUNC-DAG: Decl[InstanceVar]/CurrNominal:      property[#Int#];
// IN_SYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncFunc()[#Int#];
// IN_SYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_SYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncSendable({#arg: MySendable#})[#Int#];
// IN_SYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: asyncFunc()[' async'][#Int#];
// IN_SYNCFUNC: End completions

        let _ = self.#^IN_SYNCFUNC_SELF_DOT^#
// IN_SYNCFUNC_SELF_DOT: Begin completions
// IN_SYNCFUNC_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      property[#Int#];
// IN_SYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncFunc()[#Int#];
// IN_SYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_SYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncSendable({#arg: MySendable#})[#Int#];
// IN_SYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: asyncFunc()[' async'][#Int#];
// IN_SYNCFUNC_SELF_DOT-DAG: End completions

        let _ = self#^IN_SYNCFUNC_SELF_NODOT^#
// IN_SYNCFUNC_SELF_NODOT: Begin completions
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .property[#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncFunc()[#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncSendable({#arg: MySendable#})[#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .asyncFunc()[' async'][#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#(idx): Int#}][#Int#];
// IN_SYNCFUNC_SELF_NODOT-DAG: End completions

        let _ = other.#^IN_SYNCFUNC_OTHER_DOT^#
// IN_SYNCFUNC_OTHER_DOT: Begin completions
// IN_SYNCFUNC_OTHER_DOT-DAG: Decl[InstanceVar]/CurrNominal/NotRecommended: property[#Int#][' async'];
// IN_SYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: syncFunc()[' async'][#Int#];
// IN_SYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: syncNonSendable({#arg: MyNonSendable#})[' async'][#Int#];
// IN_SYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: syncSendable({#arg: MySendable#})[' async'][#Int#];
// IN_SYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: asyncFunc()[' async'][#Int#];
// IN_SYNCFUNC_OTHER_DOT: End completions

        let _ = other#^IN_SYNCFUNC_OTHER_NODOT^#
// IN_SYNCFUNC_OTHER_NODOT: Begin completions
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceVar]/CurrNominal/NotRecommended: .property[#Int#][' async'];
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .syncFunc()[' async'][#Int#];
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .syncNonSendable({#arg: MyNonSendable#})[' async'][#Int#];
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .syncSendable({#arg: MySendable#})[' async'][#Int#];
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .asyncFunc()[' async'][#Int#];
// IN_SYNCFUNC_OTHER_NODOT-DAG: Decl[Subscript]/CurrNominal/NotRecommended: [{#(idx): Int#}][' async'][#Int#];
// IN_SYNCFUNC_OTHER_NODOT: End completions
    }
    func testAsyncFunc(other: MyActor) async {
        let _ = #^IN_ASYNCFUNC^#
// IN_ASYNCFUNC: Begin completions
// IN_ASYNCFUNC-DAG: Decl[InstanceVar]/CurrNominal:      property[#Int#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncFunc()[#Int#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   syncSendable({#arg: MySendable#})[#Int#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   asyncFunc()[' async'][#Int#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   testSyncFunc({#other: MyActor#})[#Void#];
// IN_ASYNCFUNC-DAG: Decl[InstanceMethod]/CurrNominal:   testAsyncFunc({#other: MyActor#})[' async'][#Void#];
// IN_ASYNCFUNC: End completions
        let _ = self.#^IN_ASYNCFUNC_SELF_DOT^#
// IN_ASYNCFUNC_SELF_DOT: Begin completions
// IN_ASYNCFUNC_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      property[#Int#];
// IN_ASYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncFunc()[#Int#];
// IN_ASYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_ASYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncSendable({#arg: MySendable#})[#Int#];
// IN_ASYNCFUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   asyncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_SELF_DOT: End completions

        let _ = self#^IN_ASYNCFUNC_SELF_NODOT^#
// IN_ASYNCFUNC_SELF_NODOT: Begin completions
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .property[#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncFunc()[#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncNonSendable({#arg: MyNonSendable#})[#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncSendable({#arg: MySendable#})[#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .asyncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#(idx): Int#}][#Int#];
// IN_ASYNCFUNC_SELF_NODOT-DAG: End completions

        let _ = other.#^IN_ASYNCFUNC_OTHER_DOT^#
// IN_ASYNCFUNC_OTHER_DOT: Begin completions
// IN_ASYNCFUNC_OTHER_DOT-DAG: Decl[InstanceVar]/CurrNominal:      property[#Int#][' async'];
// IN_ASYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: syncNonSendable({#arg: MyNonSendable#})[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   syncSendable({#arg: MySendable#})[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   asyncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_DOT: End completions

        let _ = other#^IN_ASYNCFUNC_OTHER_NODOT^#
// IN_ASYNCFUNC_OTHER_NODOT: Begin completions
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .property[#Int#][' async'];
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: .syncNonSendable({#arg: MyNonSendable#})[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .syncSendable({#arg: MySendable#})[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .asyncFunc()[' async'][#Int#];
// IN_ASYNCFUNC_OTHER_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#(idx): Int#}][' async'][#Int#];
// IN_ASYNCFUNC_OTHER_NODOT: End completions
    }
}

