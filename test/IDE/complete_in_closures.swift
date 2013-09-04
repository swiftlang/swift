// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}
}

// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// FOO_OBJECT_DOT-NEXT: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// FOO_OBJECT_DOT-NEXT: Keyword: metatype[#FooStruct.metatype#]{{$}}
// FOO_OBJECT_DOT-NEXT: End completions

//===--- Check that we can resolve closure parameters.

func testResolveClosureParam1() {
  var x = { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_1^# }
}

func testResolveClosureParam2() {
  { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_2^# }
}

