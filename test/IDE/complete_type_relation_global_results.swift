// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ImportPath)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -o %t/ImportPath/Lib.swiftmodule -emit-module-interface-path %t/ImportPath/Lib.swiftinterface
// RUN: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE -I %t/ImportPath
// Perform the same completion again, this time using the code completion cache that implicitly gets added to swift-ide-test
// RUN2: %swift-ide-test -code-completion -source-filename %t/test.swift -code-completion-token COMPLETE -I %t/ImportPath | %FileCheck %s

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

//--- test.swift

import Lib

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