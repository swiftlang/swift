// REQUIRES: concurrency 

// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t/output -warn-concurrency

func asyncFunc() async {}
func syncFunc() {}

struct MySendable {}
class MyNonSendable {}


actor MyActor {
    func receiveSendable(arg: MySendable) {}
    func receiveNonSendable(arg: MyNonSendable) {}
}

func testAsyncContext() {
    #^SYNC_CONTEXT^#
// SYNC_CONTEXT: Begin completions
// SYNC_CONTEXT-DAG: Decl[FreeFunction]/CurrModule/NotRecommended: asyncFunc()[' async'][#Void#]; name=asyncFunc(); diagnostics=error:async 'asyncFunc()' used in a context that does not support concurrency{{$}}
// SYNC_CONTEXT-DAG: Decl[FreeFunction]/CurrModule:      syncFunc()[#Void#]; name=syncFunc(){{$}}
// SYNC_CONTEXT: End completions
}

func testActor(obj: MyActor) async {
    obj.#^ACTOR^#
// ACTOR: Begin completions
// ACTOR-DAG: Decl[InstanceMethod]/CurrNominal:   receiveSendable({#arg: MySendable#})[' async'][#Void#]; name=receiveSendable(arg:){{$}}
// ACTOR-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: receiveNonSendable({#arg: MyNonSendable#})[' async'][#Void#]; name=receiveNonSendable(arg:); diagnostics=warning:actor-isolated 'receiveNonSendable(arg:)' should only be referenced from inside the actor{{$}}
// ACTOR: End completions
}

func testClosure(obj: (Int) async -> Void) {
  obj(#^CLOSURE_CALL^#)
// FIXME: Emit diagnostics
// CLOSURE_CALL: Begin completions
// CLOSURE_CALL-DAG: Pattern/CurrModule/Flair[ArgLabels]/NotRecommended: ['(']{#Int#}[')'][' async'][#Void#]; name={{$}}
// CLOSURE_CALL: End completions
}
