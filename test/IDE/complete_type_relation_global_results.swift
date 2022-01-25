// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ImportPath)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/ImportPath/Lib.swiftmodule -emit-module-interface-path %t/ImportPath/Lib.swiftinterface

// BEGIN Lib.swift

public protocol MyProto {}

public protocol MyOtherProto {}

public struct Foo: MyProto {
	public init() {}
}
public struct Bar: MyOtherProto {
	public init() {}
}
public struct FooBar: MyProto, MyOtherProto {
	public init() {}
}

public func makeFoo() -> Foo {
	return Foo()
}

public let GLOBAL_FOO = Foo()

// BEGIN test.swift

import Lib

// RUN: %empty-directory(%t/completion-cache)
// RUN: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE -completion-cache-path %t/completion-cache -I %t/ImportPath | %FileCheck %s
// Perform the same completion again, this time using the code completion cache that implicitly gets added to swift-ide-test
// RUN: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE -completion-cache-path %t/completion-cache -I %t/ImportPath | %FileCheck %s

func test() -> MyProto {
	return #^COMPLETE^#
}

// CHECK: Begin completions
// CHECK-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: Foo[#Foo#];
// CHECK-DAG: Decl[GlobalVar]/OtherModule[Lib]/TypeRelation[Convertible]: GLOBAL_FOO[#Foo#];
// CHECK-DAG: Decl[Struct]/OtherModule[Lib]:      Bar[#Bar#];
// CHECK-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]/TypeRelation[Identical]: MyProto[#MyProto#];
// CHECK-DAG: Decl[FreeFunction]/OtherModule[Lib]/TypeRelation[Convertible]: makeFoo()[#Foo#];
// CHECK: End completions


// RUN: %empty-directory(%t/completion-cache)
// RUN: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE_OPAQUE_COMPOSITION -completion-cache-path %t/completion-cache -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_OPAQUE_COMPOSITION
// Perform the same completion again, this time using the code completion cache that implicitly gets added to swift-ide-test
// RUN: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE_OPAQUE_COMPOSITION -completion-cache-path %t/completion-cache -I %t/ImportPath | %FileCheck %s --check-prefix=COMPLETE_OPAQUE_COMPOSITION

func testOpaqueComposition() -> some MyProto & MyOtherProto {
	return #^COMPLETE_OPAQUE_COMPOSITION^#
}

// COMPLETE_OPAQUE_COMPOSITION: Begin completions
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[Struct]/OtherModule[Lib]: Foo[#Foo#];
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[GlobalVar]/OtherModule[Lib]: GLOBAL_FOO[#Foo#];
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[Struct]/OtherModule[Lib]:      Bar[#Bar#];
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[Protocol]/OtherModule[Lib]/Flair[RareType]: MyProto[#MyProto#];
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[FreeFunction]/OtherModule[Lib]: makeFoo()[#Foo#];
// COMPLETE_OPAQUE_COMPOSITION-DAG: Decl[Struct]/OtherModule[Lib]/TypeRelation[Convertible]: FooBar[#FooBar#];
// COMPLETE_OPAQUE_COMPOSITION: End completions
