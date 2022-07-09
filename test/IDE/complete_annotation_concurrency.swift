// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -code-completion-annotate-results -code-completion-sourcetext -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

// REQUIRES: concurrency

public protocol MyActorProto: Actor {
    public nonisolated func nonIsolatedFunc()
}

public actor MyActor1: MyActorProto {
  #^CONFORM_ACTORPROTO_WITHOUTINTRO^#
// CONFORM_ACTORPROTO_WITHOUTINTRO: Begin completions
// CONFORM_ACTORPROTO_WITHOUTINTRO-DAG: Decl[InstanceMethod]/Super:         <name>nonIsolatedFunc</name>(); typename=; name=nonIsolatedFunc(); sourcetext=public nonisolated func nonIsolatedFunc() {\n<#code#>\n}
// CONFORM_ACTORPROTO_WITHOUTINTRO: End completions
}

public actor MyActor2: MyActorProto {
  public func #^CONFORM_ACTORPROTO_WITHINTROACCESS^#
// NOTE: 'nonisolated' is missing, but the user can add it after the completion.
// CONFORM_ACTORPROTO_WITHINTROACCESS: Begin completions
// CONFORM_ACTORPROTO_WITHINTROACCESS-DAG: Decl[InstanceMethod]/Super:         <name>nonIsolatedFunc</name>(); typename=; name=nonIsolatedFunc(); sourcetext=nonIsolatedFunc() {\n<#code#>\n}
// CONFORM_ACTORPROTO_WITHINTROACCESS: End completions
}

public actor MyActor3: MyActorProto {
  func #^CONFORM_ACTORPROTO_WITHINTRO^#
// NOTE: Since missing 'public' is super common, code completion automatically add it.
// CONFORM_ACTORPROTO_WITHINTRO: Begin completions
// CONFORM_ACTORPROTO_WITHINTRO-DAG: Decl[InstanceMethod]/Super/Erase[5]: <name>nonIsolatedFunc</name>(); typename=; name=nonIsolatedFunc(); sourcetext=public nonisolated func nonIsolatedFunc() {\n<#code#>\n}
// CONFORM_ACTORPROTO_WITHINTRO: End completions
}
