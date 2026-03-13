// REQUIRES: concurrency
// RUN: %batch-code-completion

class MyNonSendable {}
struct MySendable {}

@globalActor
actor MyGlobalActor {
  static var shared = MyGlobalActor()
}

@globalActor
actor MyOtherGlobalActor {
  static var shared = MyOtherGlobalActor()
}

@MyGlobalActor func globalFuncOnGlobalActor() {}

func takeClosure<T>(_: () async -> T) {}

var otherInstanceOfMyClass = MyClass()

class MyClass {
  @MyGlobalActor func funcOnGlobalActor() -> Int { return 0 }
  @MyOtherGlobalActor func funcOnOtherGlobalActor() -> Int { return 0 }
  func funcSync() -> Int { return 0 }
  
  @MyGlobalActor func nonSenableFuncOnGlobalActor(arg: MyNonSendable) -> Int { return 0 }
  @MyOtherGlobalActor func nonSenableFuncOnOtherGlobalActor(arg: MyNonSendable) -> Int { return 0 }

  @MyGlobalActor var varOnGlobalActor: Int = 0
  @MyOtherGlobalActor var varOnOtherGlobalActor: Int = 0
  var varSync: Int = 0

  @MyGlobalActor subscript(onGlobalActor onGlobalActor: Int) -> Int { get { 1 } set { } }
  @MyOtherGlobalActor subscript(onOtherGlobalActor onOtherGlobalActor: Int) -> Int { get { 1 } set { } }
  subscript(sync sync: Int) -> Int { get { 1 } set { } }
}

extension MyClass {
  @MyGlobalActor func testOnGlobalActor() {
    let _ = #^IN_FUNC_ON_GLOBAL_ACTOR^#
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceMethod]/CurrNominal:   funcOnGlobalActor()[#Int#]; name=funcOnGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceMethod]/CurrNominal: funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceMethod]/CurrNominal:   funcSync()[#Int#]; name=funcSync()
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceMethod]/CurrNominal:   nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceMethod]/CurrNominal: nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceVar]/CurrNominal:      varOnGlobalActor[#Int#]; name=varOnGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceVar]/CurrNominal: varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR-DAG: Decl[InstanceVar]/CurrNominal:      varSync[#Int#]; name=varSync

    let _ = self.#^IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcOnGlobalActor()[#Int#]; name=funcOnGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal: funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcSync()[#Int#]; name=funcSync()
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal: nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varOnGlobalActor[#Int#]; name=varOnGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal: varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varSync[#Int#]; name=varSync

    let _ = self#^IN_FUNC_ON_GLOBAL_ACTOR_NODOT^#
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .funcOnGlobalActor()[#Int#]; name=funcOnGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceMethod]/CurrNominal: .funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .funcSync()[#Int#]; name=funcSync()
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceMethod]/CurrNominal: .nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .varOnGlobalActor[#Int#]; name=varOnGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceVar]/CurrNominal: .varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .varSync[#Int#]; name=varSync
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#onGlobalActor: Int#}][#Int#]; name=[onGlobalActor:]
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[Subscript]/CurrNominal: [{#onOtherGlobalActor: Int#}][' async'][#Int#]; name=[onOtherGlobalActor:]
// IN_FUNC_ON_GLOBAL_ACTOR_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#sync: Int#}][#Int#]; name=[sync:]

    let _ = otherInstanceOfMyClass.#^IN_FUNC_ON_GLOBAL_ACTOR_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
    let _ = otherInstanceOfMyClass#^IN_FUNC_ON_GLOBAL_ACTOR_OTHER_NODOT?check=IN_FUNC_ON_GLOBAL_ACTOR_NODOT^#
  }

  func testInSyncFunc() {
    let _ = #^IN_SYNC_FUNC^#
// IN_SYNC_FUNC-DAG: Decl[InstanceMethod]/CurrNominal:   funcOnGlobalActor()[' async'][#Int#]; name=funcOnGlobalActor()
// IN_SYNC_FUNC-DAG: Decl[InstanceMethod]/CurrNominal: funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_SYNC_FUNC-DAG: Decl[InstanceMethod]/CurrNominal:   funcSync()[#Int#]; name=funcSync()
// IN_SYNC_FUNC-DAG: Decl[InstanceMethod]/CurrNominal:   nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_SYNC_FUNC-DAG: Decl[InstanceMethod]/CurrNominal: nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_SYNC_FUNC-DAG: Decl[InstanceVar]/CurrNominal:      varOnGlobalActor[#Int#][' async']; name=varOnGlobalActor
// IN_SYNC_FUNC-DAG: Decl[InstanceVar]/CurrNominal: varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_SYNC_FUNC-DAG: Decl[InstanceVar]/CurrNominal:      varSync[#Int#]; name=varSync

    let _ = self.#^IN_SYNC_FUNC_SELF_DOT^#
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcOnGlobalActor()[' async'][#Int#]; name=funcOnGlobalActor()
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal: funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcSync()[#Int#]; name=funcSync()
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceMethod]/CurrNominal: nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varOnGlobalActor[#Int#][' async']; name=varOnGlobalActor
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal: varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_SYNC_FUNC_SELF_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varSync[#Int#]; name=varSync

    let _ = self#^IN_SYNC_FUNC_NODOT^#
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .funcOnGlobalActor()[' async'][#Int#]; name=funcOnGlobalActor()
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceMethod]/CurrNominal: .funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .funcSync()[#Int#]; name=funcSync()
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceMethod]/CurrNominal: .nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .varOnGlobalActor[#Int#][' async']; name=varOnGlobalActor
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceVar]/CurrNominal: .varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_SYNC_FUNC_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .varSync[#Int#]; name=varSync
// IN_SYNC_FUNC_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#onGlobalActor: Int#}][' async'][#Int#]; name=[onGlobalActor:]
// IN_SYNC_FUNC_NODOT-DAG: Decl[Subscript]/CurrNominal: [{#onOtherGlobalActor: Int#}][' async'][#Int#]; name=[onOtherGlobalActor:]
// IN_SYNC_FUNC_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#sync: Int#}][#Int#]; name=[sync:]

    let _ = otherInstanceOfMyClass.#^IN_SYNC_FUNC_OTHER_DOT?check=IN_SYNC_FUNC_SELF_DOT^#
    let _ = otherInstanceOfMyClass#^IN_SYNC_FUNC_OTHER_NODOT?check=IN_SYNC_FUNC_NODOT^#
  }

  func testInGlobalActorClosure() {
    _ = { @MyGlobalActor () -> Void in
      let _ = otherInstanceOfMyClass.#^IN_CLOSURE_ON_GLOBAL_ACTOR_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
    }
  }

  func testInGlobalActorClosureWithoutExplicitAttribute() {
    let callback: @MyGlobalActor () -> Void
    callback = {
      let _ = otherInstanceOfMyClass.#^IN_CLOSURE_ON_GLOBAL_ACTOR_WITHOUT_EXPLICIT_LABEL_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
    }
  }

  @MyGlobalActor func testInClosureInGlobalActorFunc() {
    _ = { () -> Void in
      let _ = otherInstanceOfMyClass.#^IN_CLOSURE_IN_FUNC_ON_GLOBAL_ACTOR_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
    }
  }

  func testInClosureNestedInClosureOnGlobalActorFunc() {
    _ = { @MyGlobalActor () -> Void in
      _ = { () -> Void in
        let _ = otherInstanceOfMyClass.#^IN_CLOSURE_NESTED_IN_CLOSURE_ON_GLOBAL_ACTOR_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
      }
    }
  }

  func testInLocalFunc() {
    @MyGlobalActor func localFunc() {
      let _ = otherInstanceOfMyClass.#^IN_LOCAL_FUNC_ON_GLOBAL_ACTOR_OTHER_DOT?check=IN_FUNC_ON_GLOBAL_ACTOR_SELF_DOT^#
    }
  }

  @MyGlobalActor func testInNestedSingleExpressionClosure() {
    takeClosure {
      takeClosure {
        otherInstanceOfMyClass.#^IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT^#
      }
    }
  }
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcOnGlobalActor()[#Int#]; name=funcOnGlobalActor()
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal: funcOnOtherGlobalActor()[' async'][#Int#]; name=funcOnOtherGlobalActor()
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   funcSync()[#Int#]; name=funcSync()
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   nonSenableFuncOnGlobalActor({#arg: MyNonSendable#})[#Int#]; name=nonSenableFuncOnGlobalActor(arg:)
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceMethod]/CurrNominal: nonSenableFuncOnOtherGlobalActor({#arg: MyNonSendable#})[' async'][#Int#]; name=nonSenableFuncOnOtherGlobalActor(arg:)
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varOnGlobalActor[#Int#]; name=varOnGlobalActor
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceVar]/CurrNominal: varOnOtherGlobalActor[#Int#][' async']; name=varOnOtherGlobalActor
// IN_NESTED_SINGLE_EXPRESSION_CLOSURE_ON_GLBOAL_ACTOR_OTHER_DOT-DAG: Decl[InstanceVar]/CurrNominal:      varSync[#Int#]; name=varSync
}

actor ActorTests {
  func testInActor() {
    let _ = otherInstanceOfMyClass.#^IN_ACTOR_OTHER_DOT?check=IN_SYNC_FUNC_SELF_DOT^#
    let _ = otherInstanceOfMyClass#^IN_ACTOR_OTHER_NODOT?check=IN_SYNC_FUNC_NODOT^#
  }
}
