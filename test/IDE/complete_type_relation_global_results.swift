// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ImportPath)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/ImportPath/Lib.swiftmodule -emit-module-interface-path %t/ImportPath/Lib.swiftinterface

//--- Lib.swift

public protocol MyProto {}

public struct Foo: MyProto {
	public init() {}
}
public struct Bar {
	public init() {}
}

public func makeFoo() -> Foo {
	return Foo()
}

public let GLOBAL_FOO = Foo()

public class MyClass {}
public class MySubclass: MyClass {}

//--- test.swift

import Lib

// This extension should not make Bar convertible to MyProto because Bar's conformance to MyProto can't be cached since that cache might also be used by other modules.
extension Bar: MyProto {}

func testWithProtocolContext() -> MyProto {
	return #^COMPLETE_PROTOCOL_CONTEXT^#
}
// RUN: %empty-directory(%t/protocol_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/protocol_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_PROTOCOL_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_PROTOCOL_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/protocol_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_PROTOCOL_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_PROTOCOL_CONTEXT

// COMPLETE_PROTOCOL_CONTEXT: Begin completions
// COMPLETE_PROTOCOL_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]:      Bar[#Bar#];
// COMPLETE_PROTOCOL_CONTEXT-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]/TypeRelation[Identical]: MyProto[#MyProto#];
// COMPLETE_PROTOCOL_CONTEXT-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT: End completions

func testWithOpaqueResultTypeContext() -> some MyProto {
	return #^COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT^#
}
// RUN: %empty-directory(%t/opaque_result_type_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/opaque_result_type_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_PROTOCOL_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/opaque_result_type_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_PROTOCOL_CONTEXT

// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT: Begin completions
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]:      Bar[#Bar#];
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]/TypeRelation[Identical]: MyProto[#MyProto#];
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// COMPLETE_OPAQUE_RESULT_TYPE_CONTEXT: End completions

func testWithOptionalProtocolContext() -> MyProto? {
	return #^COMPLETE_OPTIONAL_PROTOCOL_CONTEXT^#
}
// RUN: %empty-directory(%t/optional_protocol_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/optional_protocol_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPTIONAL_PROTOCOL_CONTEXT -I %t/ImportPath  | %FileCheck %s --check-prefix=COMPLETE_OPTIONAL_PROTOCOL_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/optional_protocol_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPTIONAL_PROTOCOL_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_OPTIONAL_PROTOCOL_CONTEXT

// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT: Begin completions
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]:      Bar[#Bar#];
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]/TypeRelation[Convertible]: MyProto[#MyProto#];
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// COMPLETE_OPTIONAL_PROTOCOL_CONTEXT: End completions

func testWithStructContext() -> Foo {
	return #^COMPLETE_STRUCT_CONTEXT^#
}
// RUN: %empty-directory(%t/struct_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/struct_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_STRUCT_CONTEXT -I %t/ImportPath  | %FileCheck %s --check-prefix=COMPLETE_STRUCT_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/struct_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_STRUCT_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_STRUCT_CONTEXT

// COMPLETE_STRUCT_CONTEXT: Begin completions
// COMPLETE_STRUCT_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Identical]: Foo[#Foo#];
// COMPLETE_STRUCT_CONTEXT-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Identical]: GLOBAL_FOO[#Foo#];
// COMPLETE_STRUCT_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]:      Bar[#Bar#];
// COMPLETE_STRUCT_CONTEXT-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]: MyProto[#MyProto#];
// COMPLETE_STRUCT_CONTEXT-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Identical]: makeFoo()[#Foo#];
// COMPLETE_STRUCT_CONTEXT: End completions

func testWithOptionalStructContext() -> Foo? {
	return #^COMPLETE_OPTIONAL_STRUCT_CONTEXT^#
}
// RUN: %empty-directory(%t/optional_struct_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/optional_struct_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPTIONAL_STRUCT_CONTEXT -I %t/ImportPath  | %FileCheck %s --check-prefix=COMPLETE_OPTIONAL_STRUCT_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/optional_struct_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_OPTIONAL_STRUCT_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_OPTIONAL_STRUCT_CONTEXT

// COMPLETE_OPTIONAL_STRUCT_CONTEXT: Begin completions
// COMPLETE_OPTIONAL_STRUCT_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// COMPLETE_OPTIONAL_STRUCT_CONTEXT-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// COMPLETE_OPTIONAL_STRUCT_CONTEXT-DAG: Decl[Struct]/OtherModule[Lib]:      Bar[#Bar#];
// COMPLETE_OPTIONAL_STRUCT_CONTEXT-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]: MyProto[#MyProto#];
// COMPLETE_OPTIONAL_STRUCT_CONTEXT-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// COMPLETE_OPTIONAL_STRUCT_CONTEXT: End completions

func testWithSuperclassContext() -> MyClass? {
	return #^COMPLETE_SUPERCLASS_CONTEXT^#
}
// RUN: %empty-directory(%t/superclass_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/superclass_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_SUPERCLASS_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_SUPERCLASS_CONTEXT
// Perform the same completion again, this time using the code completion cache
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/superclass_context_cache -source-filename %t/test.swift -code-completion-token COMPLETE_SUPERCLASS_CONTEXT -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_SUPERCLASS_CONTEXT

// COMPLETE_SUPERCLASS_CONTEXT: Begin completions
// COMPLETE_SUPERCLASS_CONTEXT-DAG: Decl[Class]/OtherModule[Lib]/TypeRelation[Convertible]: MySubclass[#MySubclass#];
// COMPLETE_SUPERCLASS_CONTEXT-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: testWithSuperclassContext()[#MyClass?#];
// COMPLETE_SUPERCLASS_CONTEXT: End completions

//--- test2.swift

// Make sure that we don't cache the conformance Bar: MyProto because it's not declared inside Lib and thus doesn't apply to test2.swift

import Lib

func testWithProtocolContextWithoutExtension() -> MyProto {
	return #^COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION^#
}
// RUN: %empty-directory(%t/protocol_context_cache)
// RUN: %swift-ide-test_plain -code-completion -completion-cache-path %t/protocol_context_cache -source-filename %t/test2.swift -code-completion-token COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION

// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION: Begin completions
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION-DAG: Decl[Struct]/OtherModule[Lib]:      Bar[#Bar#];
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]/TypeRelation[Identical]: MyProto[#MyProto#];
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// COMPLETE_PROTOCOL_CONTEXT_WITHOUT_PROTOCOL_EXTENSION: End completions