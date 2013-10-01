// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/dynamic_lookup_foo_swift_module.swift

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_FUNC_PARAM_NO_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_NO_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_FUNC_PARAM_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_VAR_NO_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_NO_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_VAR_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_RETURN_VAL_NO_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_NO_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_RETURN_VAL_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_INSTANCE_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_CLASS_NO_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_CLASS_NO_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -I %t -code-completion-token=DL_CLASS_DOT_1 > %t.dl.txt
// RUN: FileCheck %s -check-prefix=DL_CLASS_DOT < %t.dl.txt
// RUN: FileCheck %s -check-prefix=GLOBAL_NEGATIVE < %t.dl.txt


import dynamic_lookup_foo_swift_module

//===---
//===--- Helper types that are used in this test.
//===---

class Base {}
class Derived : Base {}

protocol Foo { func foo() }
protocol Bar { func bar() }

//===---
//===--- Types that contain members accessible by dynamic lookup.
//===---

// GLOBAL_NEGATIVE-NOT: ERROR

// DL_INSTANCE_NO_DOT: Begin completions, 13 items
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc2{#?|!#}({#a: Derived#})[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc3{#?|!#}({#a: Derived#})[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc4{#?|!#}()[#Base#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .foo_Nested1_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .foo_Nested2_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .foo_TopLevelClass_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .foo_TopLevelObjcClass_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .nested1_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .nested2_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .topLevelClass_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: SwiftDecl: .topLevelObjcClass_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_NO_DOT-DAG: Keyword: .metatype[#DynamicLookup.metatype#]{{$}}
// DL_INSTANCE_NO_DOT: End completions

// DL_INSTANCE_DOT: Begin completions, 13 items
// DL_INSTANCE_DOT-DAG: SwiftDecl: base1_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: base1_InstanceFunc2{#?|!#}({#a: Derived#})[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: base1_InstanceFunc3{#?|!#}({#a: Derived#})[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: base1_InstanceFunc4{#?|!#}()[#Base#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: foo_Nested1_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: foo_Nested2_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: foo_TopLevelClass_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: foo_TopLevelObjcClass_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: nested1_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: nested2_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: topLevelClass_ObjcInstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: SwiftDecl: topLevelObjcClass_InstanceFunc1{#?|!#}()[#Void#]{{$}}
// DL_INSTANCE_DOT-DAG: Keyword: metatype[#DynamicLookup.metatype#]{{$}}
// DL_INSTANCE_DOT: End completions

// DL_CLASS_NO_DOT: Begin completions, 21 items
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc1({#self: Base1#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc2({#self: Base1#})[#(a: Derived) -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc3({#self: Base1#})[#(a: Derived) -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .base1_InstanceFunc4({#self: Base1#})[#() -> Base#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_Nested1_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_Nested1_ObjcInstanceFunc1({#self: Foo_ContainerForNestedClass1.Foo_Nested1#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_Nested2_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_Nested2_ObjcInstanceFunc1({#self: Foo_ContainerForNestedClass2.Foo_Nested2#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_TopLevelClass_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_TopLevelClass_ObjcInstanceFunc1({#self: Foo_TopLevelClass#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_TopLevelObjcClass_ClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .foo_TopLevelObjcClass_InstanceFunc1({#self: Foo_TopLevelObjcClass#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .nested1_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .nested1_ObjcInstanceFunc1({#self: ContainerForNestedClass1.Nested1#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .nested2_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .nested2_ObjcInstanceFunc1({#self: ContainerForNestedClass2.Nested2#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .topLevelClass_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .topLevelClass_ObjcInstanceFunc1({#self: TopLevelClass#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .topLevelObjcClass_ClassFunc1()[#Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: SwiftDecl: .topLevelObjcClass_InstanceFunc1({#self: TopLevelObjcClass#})[#() -> Void#]{{$}}
// DL_CLASS_NO_DOT-DAG: Keyword: .metatype[#DynamicLookup.metatype.metatype#]{{$}}
// DL_CLASS_NO_DOT: End completions

// DL_CLASS_DOT: Begin completions, 21 items
// DL_CLASS_DOT-DAG: SwiftDecl: base1_InstanceFunc1({#self: Base1#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: base1_InstanceFunc2({#self: Base1#})[#(a: Derived) -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: base1_InstanceFunc3({#self: Base1#})[#(a: Derived) -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: base1_InstanceFunc4({#self: Base1#})[#() -> Base#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_Nested1_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_Nested1_ObjcInstanceFunc1({#self: Foo_ContainerForNestedClass1.Foo_Nested1#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_Nested2_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_Nested2_ObjcInstanceFunc1({#self: Foo_ContainerForNestedClass2.Foo_Nested2#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_TopLevelClass_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_TopLevelClass_ObjcInstanceFunc1({#self: Foo_TopLevelClass#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_TopLevelObjcClass_ClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: foo_TopLevelObjcClass_InstanceFunc1({#self: Foo_TopLevelObjcClass#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: nested1_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: nested1_ObjcInstanceFunc1({#self: ContainerForNestedClass1.Nested1#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: nested2_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: nested2_ObjcInstanceFunc1({#self: ContainerForNestedClass2.Nested2#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: topLevelClass_ObjcClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: topLevelClass_ObjcInstanceFunc1({#self: TopLevelClass#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: topLevelObjcClass_ClassFunc1()[#Void#]{{$}}
// DL_CLASS_DOT-DAG: SwiftDecl: topLevelObjcClass_InstanceFunc1({#self: TopLevelObjcClass#})[#() -> Void#]{{$}}
// DL_CLASS_DOT-DAG: Keyword: metatype[#DynamicLookup.metatype.metatype#]{{$}}
// DL_CLASS_DOT: End completions


// TODO: add [objc,class_protocol] protocols

// TODO: properties
// TODO: subscript operators
// TODO: static variables

class [objc] TopLevelObjcClass {
  func topLevelObjcClass_InstanceFunc1() {}
  static func topLevelObjcClass_ClassFunc1() {}
}

class TopLevelClass {
  func [objc] topLevelClass_ObjcInstanceFunc1() {}
  static func [objc] topLevelClass_ObjcClassFunc1() {}

  func ERROR() {}
}

class ContainerForNestedClass1 {
  class Nested1 {
    func [objc] nested1_ObjcInstanceFunc1() {}
    static func [objc] nested1_ObjcClassFunc1() {}
    func ERROR() {}
  }
  func ERROR() {}
}

struct ContainerForNestedClass2 {
  class Nested2 {
    func [objc] nested2_ObjcInstanceFunc1() {}
    static func [objc] nested2_ObjcClassFunc1() {}
    func ERROR() {}
  }
  func ERROR() {}
}

class GenericContainerForNestedClass1<T> {
  class Nested3 {
    func [objc] ERROR1() {}
    func ERROR2() {}
  }
  func ERROR() {}
}

struct GenericContainerForNestedClass2<T> {
  class Nested3 {
    func [objc] ERROR1() {}
    func ERROR2() {}
  }
  func ERROR() {}
}

class [objc] Base1 {
  func base1_InstanceFunc1() {}

  func base1_InstanceFunc2(a: Derived) {}

  func base1_InstanceFunc3(a: Derived) {}

  func base1_InstanceFunc4() -> Base {}
}

class [objc] Derived1 : Base1 {
  func base1_InstanceFunc1() {}

  func base1_InstanceFunc2(a: Derived) {}

  func base1_InstanceFunc3(a: Base) {}

  func base1_InstanceFunc4() -> Derived {}
}

func returnsDynamicLookup() -> DynamicLookup {
  return TopLevelClass()
}

func testDynamicLookup1(dl: DynamicLookup) {
  dl#^DL_FUNC_PARAM_NO_DOT_1^#
}

func testDynamicLookup2(dl: DynamicLookup) {
  dl.#^DL_FUNC_PARAM_DOT_1^#
}

func testDynamicLookup3() {
  var dl: DynamicLookup = TopLevelClass()
  dl#^DL_VAR_NO_DOT_1^#
}

func testDynamicLookup4() {
  var dl: DynamicLookup = TopLevelClass()
  dl.#^DL_VAR_DOT_1^#
}

func testDynamicLookup5() {
  returnsDynamicLookup()#^DL_RETURN_VAL_NO_DOT_1^#
}

func testDynamicLookup6() {
  returnsDynamicLookup().#^DL_RETURN_VAL_DOT_1^#
}

func testDynamicLookup7() {
  // TODO: completion after dl.foo!()
}

func testDynamicLookup8() {
  // TODO: completion after dl.foo?()
}

func testDynamicLookup9() {
  // TODO: completion after dl.foo!
}

func testDynamicLookup10() {
  // TODO: completion after dl.foo!
}

func testDynamicLookup11() {
  // TODO: completion after dl.foo?
}

func testDynamicLookupClassMethods1(dl: DynamicLookup) {
  typeof(dl)#^DL_CLASS_NO_DOT_1^#
}

func testDynamicLookupClassMethods2(dl: DynamicLookup) {
  typeof(dl).#^DL_CLASS_DOT_1^#
}

