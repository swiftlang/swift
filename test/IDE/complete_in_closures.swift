// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_1 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_2 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_3 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_4 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_5 | FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_6 | FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=ARRAY_INIT_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_1 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_1 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_2 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_3 | FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_4 | FileCheck %s -check-prefix=ERROR_COMMON

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}
}

// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{$}}
// FOO_OBJECT_DOT-NEXT: End completions

// WITH_GLOBAL_DECLS: Begin completions
// WITH_GLOBAL_DECLS: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{$}}
// WITH_GLOBAL_DECLS: End completions

//===--- Check that we can resolve closure parameters.

func testResolveClosureParam1() {
  var x = { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_1^# }
}

func testResolveClosureParam2() {
  { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_2^# }
}

//===--- Check that we can resolve parent function parameters.

func testResolveParentParam1(fs: FooStruct) {
  { (a: Int) in fs.#^RESOLVE_PARENT_PARAM_1^# }
}

func testResolveParentParam2(fs: FooStruct) {
  { fs.#^RESOLVE_PARENT_PARAM_2^# }
}

class TestResolveParentParam3 {
  func testResolveParentParam3a(fs: FooStruct) {
    { (a: Int) in fs.#^RESOLVE_PARENT_PARAM_3^# }
  }
}

class TestResolveParentParam4 {
  func testResolveParentParam4a(fs: FooStruct) {
    { fs.#^RESOLVE_PARENT_PARAM_4^# }
  }
}

func testResolveParentParam5(fs: FooStruct) {
  func testResolveParentParam5a() {
    { fs.#^RESOLVE_PARENT_PARAM_5^# }
  }
}

func testResolveParentParam6() {
  func testResolveParentParam6a(fs: FooStruct) {
    { fs.#^RESOLVE_PARENT_PARAM_6^# }
  }
}

//===--- Test completion in array initializer closures.

func testArrayInit1() {
  var a = new Int[42] { #^ARRAY_INIT_1^# }
}

//===--- Test completion in various statements in closures.

func testReturnInClosure1() {
  var f = { () -> Int in
    return #^RETURN_1^#
  }
}

//===--- Test that we do delayed parsing of closures.
// FIXME: <rdar://problem/16274593> Code completion: implement delayed parsing of closures

var topLevelClosure1 = { #^DELAYED_1^# }

var topLevelClosure2 = { func f() { #^DELAYED_2^# } }

var topLevelClosure3 = { class C { func f() { #^DELAYED_3^# } } }

class ClassWithClosureMember1 {
  var c1 = { #^DELAYED_4^# }
}

