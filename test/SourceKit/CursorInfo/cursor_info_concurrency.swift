// REQUIRES: concurrency

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/Modules/MyModule.swiftmodule -module-name MyModule %t/MyModule.swift  -target %target-swift-5.1-abi-triple

// RUN: %sourcekitd-test -req=cursor -pos=1:15 %t/MyModule.swift -- %t/MyModule.swift -target %target-triple  | %FileCheck -check-prefix=ACTOR %s
// RUN: %sourcekitd-test -req=cursor -pos=2:15 %t/MyModule.swift -- %t/MyModule.swift -target %target-triple  | %FileCheck -check-prefix=FUNC %s
// RUN: %sourcekitd-test -req=cursor -pos=5:16 %t/MyModule.swift -- %t/MyModule.swift -target %target-triple  | %FileCheck -check-prefix=ACTOR %s
// RUN: %sourcekitd-test -req=cursor -pos=6:19  %t/MyModule.swift -- %t/MyModule.swift -target %target-triple  | %FileCheck -check-prefix=FUNC %s

// ACTOR: <Declaration>public actor MyActor</Declaration>
// ACTOR: <decl.class><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>actor</syntaxtype.keyword> <decl.name>MyActor</decl.name></decl.class>

// FUNC: <Declaration>public func asyncFunc(fn: () async -&gt; <Type usr="s:s4Voida">Void</Type>) async throws</Declaration>
// FUNC: <decl.function.method.instance><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>asyncFunc</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>fn</decl.var.parameter.argument_label>: <decl.var.parameter.type>() <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.typealias usr="s:s4Voida">Void</ref.typealias></decl.function.returntype></decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> <syntaxtype.keyword>throws</syntaxtype.keyword></decl.function.method.instance>

// RUN: %sourcekitd-test -req=cursor -pos=3:16 %t/App.swift -- %t/App.swift -target %target-triple -I %t/Modules  | %FileCheck -check-prefix=ACTOR_XMOD %s
// RUN: %sourcekitd-test -req=cursor -pos=4:19 %t/App.swift -- %t/App.swift -target %target-triple -I %t/Modules  | %FileCheck -check-prefix=FUNC_XMOD %s

// ACTOR_XMOD: <Declaration>actor MyActor</Declaration>
// ACTOR_XMOD: <decl.class><syntaxtype.keyword>actor</syntaxtype.keyword> <decl.name>MyActor</decl.name></decl.class>

// FUNC_XMOD: <Declaration>func asyncFunc(fn: () async -&gt; <Type usr="s:s4Voida">Void</Type>) async throws</Declaration>
// FUNC_XMOD: <decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>asyncFunc</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>fn</decl.var.parameter.argument_label>: <decl.var.parameter.type>() <syntaxtype.keyword>async</syntaxtype.keyword> -&gt; <decl.function.returntype><ref.typealias usr="s:s4Voida">Void</ref.typealias></decl.function.returntype></decl.var.parameter.type></decl.var.parameter>) <syntaxtype.keyword>async</syntaxtype.keyword> <syntaxtype.keyword>throws</syntaxtype.keyword></decl.function.method.instance>

//--- MyModule.swift
public actor MyActor {
  public func asyncFunc(fn: () async -> Void) async throws {}
}

func test(act: MyActor) async throws {
    try await act.asyncFunc {}
}

//--- App.swift
import MyModule

func test(act: MyActor) async throws {
    try await act.asyncFunc {}
}
