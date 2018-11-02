// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_CLOSURE_PARAM_2 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_1 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_2 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_3 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_4 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_5 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RESOLVE_PARENT_PARAM_6 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_2 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_3 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_4 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_5 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_6 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_7 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_8 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_9 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DELAYED_10 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS_AND_LOCAL1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_2 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_3 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_4 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_5 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_6 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_7 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_8 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_9 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_10 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_11 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_12 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_13 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_14 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_15 | %FileCheck %s -check-prefix=FOO_OBJECT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_16 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_17 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_18 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_TRAILING_CLOSURE_19 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INHERITANCE_IN_CLOSURE_0 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_INHERITANCE_IN_CLOSURE_0 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARGUMENT_TYPE_IN_CLOSURE_0 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INHERITANCE_IN_CLOSURE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_INHERITANCE_IN_CLOSURE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARGUMENT_TYPE_IN_CLOSURE_1 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INHERITANCE_IN_CLOSURE_2 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLASS_INHERITANCE_IN_CLOSURE_2 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ARGUMENT_TYPE_IN_CLOSURE_2 | %FileCheck %s -check-prefix=WITH_GLOBAL_DECLS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_PARAM_1 | %FileCheck %s -check-prefix=CLOSURE_PARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CLOSURE_PARAM_2 | %FileCheck %s -check-prefix=CLOSURE_PARAM_2

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

//===--- Helper types that are used in this test

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}
}

// FOO_OBJECT_DOT: Begin completions
// FOO_OBJECT_DOT-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// FOO_OBJECT_DOT-NEXT: End completions

// WITH_GLOBAL_DECLS: Begin completions
// WITH_GLOBAL_DECLS: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_DECLS: End completions

//===--- Check that we can resolve closure parameters.

func testResolveClosureParam1() {
  var x = { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_1^# }
}

func testResolveClosureParam2() {
  { (fs: FooStruct) in fs.#^RESOLVE_CLOSURE_PARAM_2^# }
}

//===--- Check that we can resolve parent function parameters.

func testResolveParentParam1(_ fs: FooStruct) {
  { (a: Int) in fs.#^RESOLVE_PARENT_PARAM_1^# }
}

func testResolveParentParam2(_ fs: FooStruct) {
  { fs.#^RESOLVE_PARENT_PARAM_2^# }
}

class TestResolveParentParam3 {
  func testResolveParentParam3a(_ fs: FooStruct) {
    { (a: Int) in fs.#^RESOLVE_PARENT_PARAM_3^# }
  }
}

class TestResolveParentParam4 {
  func testResolveParentParam4a(_ fs: FooStruct) {
    { fs.#^RESOLVE_PARENT_PARAM_4^# }
  }
}

func testResolveParentParam5(_ fs: FooStruct) {
  func testResolveParentParam5a() {
    { fs.#^RESOLVE_PARENT_PARAM_5^# }
  }
}

func testResolveParentParam6() {
  func testResolveParentParam6a(_ fs: FooStruct) {
    { fs.#^RESOLVE_PARENT_PARAM_6^# }
  }
}

//===--- Test completion in various statements in closures.

func testReturnInClosure1() {
  var f = { () -> Int in
    return #^RETURN_1^#
  }
}

//===--- Test that we do delayed parsing of closures.

var topLevelClosure1 = { #^DELAYED_1^# }

var topLevelClosure2 = { func f() { #^DELAYED_2^# } }

var topLevelClosure3 = { class C { func f() { #^DELAYED_3^# } } }

class ClassWithClosureMember1 {
  var c1 = { #^DELAYED_4^# }
  lazy var c2 = { #^DELAYED_5^# }
  var c3 = ({ #^DELAYED_6^# })()
  lazy var c4 = ({ #^DELAYED_7^# })()
}

struct NestedStructWithClosureMember1 {
  struct Nested {
    var c1 = { #^DELAYED_8^# }
    lazy var c2 = { #^DELAYED_9^# }
  }
}

// WITH_GLOBAL_DECLS_AND_LOCAL1: Begin completions
// WITH_GLOBAL_DECLS_AND_LOCAL1: Decl[LocalVar]/Local: x[#Int#]
// WITH_GLOBAL_DECLS_AND_LOCAL1: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// WITH_GLOBAL_DECLS_AND_LOCAL1: End completions

struct StructWithClosureMemberAndLocal {
  var c = {
    var x = 0
    #^DELAYED_10^#
  }
}

func acceptsTrailingClosureFooVoid(_ code: (FooStruct) -> Void) {}

acceptsTrailingClosureFooVoid {
  #^IN_TRAILING_CLOSURE_1^#
}

acceptsTrailingClosureFooVoid {
  $0.#^IN_TRAILING_CLOSURE_2^#
}

acceptsTrailingClosureFooVoid {
  item in #^IN_TRAILING_CLOSURE_3^#
}

acceptsTrailingClosureFooVoid {
  item in item.#^IN_TRAILING_CLOSURE_4^#
}

acceptsTrailingClosureFooVoid {
  item in
  item.instanceFunc0()
  item.#^IN_TRAILING_CLOSURE_5^#
}

func acceptsListAndTrailingClosureFooVoid(
    _ list: [FooStruct], code: (FooStruct) -> Void) {
}

acceptsListAndTrailingClosureFooVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  #^IN_TRAILING_CLOSURE_6^#
}

acceptsListAndTrailingClosureFooVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  $0.#^IN_TRAILING_CLOSURE_7^#
}

acceptsListAndTrailingClosureFooVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in #^IN_TRAILING_CLOSURE_8^#
}

acceptsListAndTrailingClosureFooVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in item.#^IN_TRAILING_CLOSURE_9^#
}

acceptsListAndTrailingClosureFooVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in
  item.instanceFunc0()
  item.#^IN_TRAILING_CLOSURE_10^#
}

func acceptsListAndTrailingClosureTVoid<T>(_ list: [T], code: (T) -> Void) {}

acceptsListAndTrailingClosureTVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  #^IN_TRAILING_CLOSURE_11^#
}

acceptsListAndTrailingClosureTVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  $0.#^IN_TRAILING_CLOSURE_12^#
}

acceptsListAndTrailingClosureTVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in #^IN_TRAILING_CLOSURE_13^#
}

acceptsListAndTrailingClosureTVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in item.#^IN_TRAILING_CLOSURE_14^#
}

acceptsListAndTrailingClosureTVoid(
    [ FooStruct(instanceVar: 0), FooStruct(instanceVar: 0) ]) {
  item in
  item.instanceFunc0()
  item.#^IN_TRAILING_CLOSURE_15^#
}

func getInt() -> Int? { return 0 }
func testAcceptsTrailingClosureInt1() {
  acceptsTrailingClosureFooVoid { #^IN_TRAILING_CLOSURE_16^# in
    if let myvar = getInt() {
    }
  }
}
func testAcceptsTrailingClosureInt2() {
  acceptsTrailingClosureFooVoid {
    #^IN_TRAILING_CLOSURE_17^#
    if let myvar = getInt() {
    }
  }
}
func testAcceptsTrailingClosureInt3() {
  acceptsTrailingClosureFooVoid {
    if let myvar = getInt() {
    }
    #^IN_TRAILING_CLOSURE_18^#
  }
}
func testAcceptsTrailingClosureInt4() {
  acceptsTrailingClosureFooVoid {
    if let myvar = getInt() {
      #^IN_TRAILING_CLOSURE_19^#
    }
  }
}

func testTypeInClosure1() {
  acceptsTrailingClosureFooVoid {
    struct S : #^STRUCT_INHERITANCE_IN_CLOSURE_0^#
  }
}
func testTypeInClosure2() {
  acceptsTrailingClosureFooVoid {
    class S : #^CLASS_INHERITANCE_IN_CLOSURE_0^#
  }
}
func testTypeInClosure3() {
  acceptsTrailingClosureFooVoid {
    func test(_ x: #^ARGUMENT_TYPE_IN_CLOSURE_0^#
  }
}
acceptsTrailingClosureFooVoid {
  struct S : #^STRUCT_INHERITANCE_IN_CLOSURE_1^#
}
acceptsTrailingClosureFooVoid {
  class S : #^CLASS_INHERITANCE_IN_CLOSURE_1^#
}
acceptsTrailingClosureFooVoid {
  func test(_ x: #^ARGUMENT_TYPE_IN_CLOSURE_1^#
}

struct LazyVar1 {
  lazy var x: Int = {
    struct S : #^STRUCT_INHERITANCE_IN_CLOSURE_2^#
  }()
}
struct LazyVar2 {
  lazy var x: Int = {
    class S : #^CLASS_INHERITANCE_IN_CLOSURE_2^#
  }()
}
struct LazyVar3 {
  lazy var x: Int = {
    func test(_ x: #^ARGUMENT_TYPE_IN_CLOSURE_2^#
  }()
}

func closureTaker(_ theFunc:(theValue: Int) -> ()) {}
func closureTaker2(_ theFunc: (Value1: Int, Value2: Int) -> ()) {}
func testClosureParam1() {
  closureTaker { (theValue) -> () in
    #^CLOSURE_PARAM_1^#
  }
}
// CLOSURE_PARAM_1: Begin completions
// CLOSURE_PARAM_1-DAG: Decl[LocalVar]/Local:         theValue[#Int#]{{; name=.+$}}
func testClosureParam2() {
  closureTaker2 { (Value1, Value2) -> () in
    #^CLOSURE_PARAM_2^#
  }
}
// CLOSURE_PARAM_2: Begin completions
// CLOSURE_PARAM_2-DAG: Decl[LocalVar]/Local:         Value1[#Int#]{{; name=.+$}}
// CLOSURE_PARAM_2-DAG: Decl[LocalVar]/Local:         Value2[#Int#]{{; name=.+$}}
