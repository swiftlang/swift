// REQUIRES: concurrency

// BEGIN MyModule.swift

public actor MyActor {
    public init() {}
    public func actorMethod() -> Int { 1 }

    @available(*, deprecated)
    public func deprecatedMethod() {}
}

public func globalAsyncFunc() async -> Int { 1 }

@available(*, deprecated)
public func deprecatedFunc() {}

// BEGIN App.swift
import MyModule

func testSync() -> Int{
    #^GLOBAL_IN_SYNC^#
// FIXME: 'globalAsyncFunc()' *should* be "NotRecommended" because  it's 'async'
// The curently behavior is due to completion cache. We should remember
// 'async'-ness in the cache. (rdar://78317170)

// GLOBAL_IN_SYNC: Begin completions
// GLOBAL_IN_SYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]: globalAsyncFunc()[' async'][#Int#];
// GLOBAL_IN_SYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/NotRecommended: deprecatedFunc()[#Void#];
// GLOBAL_IN_SYNC-DAG: Decl[Class]/OtherModule[MyModule]:  MyActor[#MyActor#];
// GLOBAL_IN_SYNC: End completions
}
func testAsync() async -> Int {
    #^GLOBAL_IN_ASYNC^#
// GLOBAL_IN_ASYNC: Begin completions
// GLOBAL_IN_ASYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]: globalAsyncFunc()[' async'][#Int#];
// GLOBAL_IN_ASYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/NotRecommended: deprecatedFunc()[#Void#];
// GLOBAL_IN_ASYNC-DAG: Decl[Class]/OtherModule[MyModule]:  MyActor[#MyActor#];
// GLOBAL_IN_ASYNC: End completions
}
func testSyncMember(obj: MyActor) -> Int {
    obj.#^MEMBER_IN_SYNC^#
// MEMBER_IN_SYNC: Begin completions, 4 items
// MEMBER_IN_SYNC-DAG: Keyword[self]/CurrNominal:          self[#MyActor#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Identical]: actorMethod()[' async'][#Int#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedMethod()[' async'][#Void#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceVar]/CurrNominal:      unownedExecutor[#UnownedSerialExecutor#];
// MEMBER_IN_SYNC: End completions
}

func testSyncMember(obj: MyActor) async -> Int {
    obj.#^MEMBER_IN_ASYNC^#
// MEMBER_IN_ASYNC: Begin completions, 4 items
// MEMBER_IN_ASYNC-DAG: Keyword[self]/CurrNominal:          self[#MyActor#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: actorMethod()[' async'][#Int#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedMethod()[' async'][#Void#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceVar]/CurrNominal:      unownedExecutor[#UnownedSerialExecutor#];
// MEMBER_IN_ASYNC: End completions
}

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/Modules %t/MyModule.swift

// RUN: %empty-directory(%t/output)
// RUN: %empty-directory(%t/ccp)
// RUN: %empty-directory(%t/mcp)

// NOTE: Doing twice is to ensure that the completion cache is used.
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp
