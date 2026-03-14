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
// GLOBAL_IN_SYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/TypeRelation[Convertible]: globalAsyncFunc()[' async'][#Int#];
// GLOBAL_IN_SYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/NotRecommended: deprecatedFunc()[#Void#];
// GLOBAL_IN_SYNC-DAG: Decl[Actor]/OtherModule[MyModule]:  MyActor[#MyActor#];
}
func testAsync() async -> Int {
    #^GLOBAL_IN_ASYNC^#
// GLOBAL_IN_ASYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/TypeRelation[Convertible]: globalAsyncFunc()[' async'][#Int#];
// GLOBAL_IN_ASYNC-DAG: Decl[FreeFunction]/OtherModule[MyModule]/NotRecommended: deprecatedFunc()[#Void#];
// GLOBAL_IN_ASYNC-DAG: Decl[Actor]/OtherModule[MyModule]:  MyActor[#MyActor#];
}
func testSyncMember(obj: MyActor) -> Int {
    obj.#^MEMBER_IN_SYNC^#
// MEMBER_IN_SYNC: Begin completions, 11 items
// MEMBER_IN_SYNC-DAG: Keyword[self]/CurrNominal:          self[#MyActor#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: actorMethod()[' async'][#Int#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedMethod()[' async'][#Void#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceVar]/CurrNominal:      unownedExecutor[#UnownedSerialExecutor#];
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: withSerialExecutor({#(operation): (any SerialExecutor) throws(Error) -> ~Copyable##(any SerialExecutor) throws(Error) -> ~Copyable#})[' throws'][#~Copyable#]; name=withSerialExecutor(:)
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: withSerialExecutor({#(operation): (any SerialExecutor) async throws(Error) -> ~Copyable##(any SerialExecutor) async throws(Error) -> ~Copyable#})[' async'][' throws'][#~Copyable#]; name=withSerialExecutor(:)
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: preconditionIsolated()[#Void#]; name=preconditionIsolated()
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: preconditionIsolated({#(message): String#})[#Void#]; name=preconditionIsolated(:)
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assertIsolated()[#Void#]; name=assertIsolated()
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assertIsolated({#(message): String#})[#Void#]; name=assertIsolated(:)
// MEMBER_IN_SYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assumeIsolated({#(operation): (isolated MyActor) throws -> Sendable##(isolated MyActor) throws -> Sendable#})[' rethrows'][#Sendable#]; name=assumeIsolated(:)
}

func testSyncMember(obj: MyActor) async -> Int {
    obj.#^MEMBER_IN_ASYNC^#
// MEMBER_IN_ASYNC: Begin completions, 11 items
// MEMBER_IN_ASYNC-DAG: Keyword[self]/CurrNominal:          self[#MyActor#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: actorMethod()[' async'][#Int#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/CurrNominal/NotRecommended: deprecatedMethod()[' async'][#Void#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceVar]/CurrNominal:      unownedExecutor[#UnownedSerialExecutor#];
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: withSerialExecutor({#(operation): (any SerialExecutor) throws(Error) -> ~Copyable##(any SerialExecutor) throws(Error) -> ~Copyable#})[' throws'][#~Copyable#]; name=withSerialExecutor(:)
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: withSerialExecutor({#(operation): (any SerialExecutor) async throws(Error) -> ~Copyable##(any SerialExecutor) async throws(Error) -> ~Copyable#})[' async'][' throws'][#~Copyable#]; name=withSerialExecutor(:)
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: preconditionIsolated()[#Void#]; name=preconditionIsolated()
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: preconditionIsolated({#(message): String#})[#Void#]; name=preconditionIsolated(:)
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assertIsolated()[#Void#]; name=assertIsolated()
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assertIsolated({#(message): String#})[#Void#]; name=assertIsolated(:)
// MEMBER_IN_ASYNC-DAG: Decl[InstanceMethod]/Super/IsSystem: assumeIsolated({#(operation): (isolated MyActor) throws -> Sendable##(isolated MyActor) throws -> Sendable#})[' rethrows'][#Sendable#]; name=assumeIsolated(:)
}

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %empty-directory(%t/Modules)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/Modules %t/MyModule.swift -target %target-swift-5.1-abi-triple

// RUN: %empty-directory(%t/output)
// RUN: %empty-directory(%t/ccp)
// RUN: %empty-directory(%t/mcp)

// NOTE: Doing twice is to ensure that the completion cache is used.
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %t/App.swift -filecheck %raw-FileCheck -completion-output-dir %t/output -I %t/Modules -completion-cache-path %t/ccp -module-cache-path %t/mcp
