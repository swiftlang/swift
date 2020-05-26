// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_VAR_INIT | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_VAR_INIT | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_LAZY_VAR_INIT | %FileCheck %s -check-prefix=STRUCT_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_VAR_BODY | %FileCheck %s -check-prefix=STRUCT_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_VAR_BODY | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INITIALIZER_DEFAULTPARAM | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INITIALIZER_BODY | %FileCheck %s -check-prefix=STRUCT_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_FUNC_DEFAULTPARAM  | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_FUNC_BODY | %FileCheck %s -check-prefix=STRUCT_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_FUNC_DEFAULTPARAM | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_FUNC_BODY | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_SUBSCRIPT_DEFAULTPARAM | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_INSTANCE_SUBSCRIPT_BODY | %FileCheck %s -check-prefix=STRUCT_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_SUBSCRIPT_DEFAULTPARAM | %FileCheck %s -check-prefix=STRUCT_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_STATIC_SUBSCRIPT_BODY | %FileCheck %s -check-prefix=STRUCT_STATIC

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_VAR_INIT | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INSTANCE_VAR_BODY | %FileCheck %s -check-prefix=ENUM_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_VAR_BODY | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INITIALIZER_DEFAULTPARAM | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INITIALIZER_BODY | %FileCheck %s -check-prefix=ENUM_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INSTANCE_FUNC_DEFAULTPARAM  | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INSTANCE_FUNC_BODY | %FileCheck %s -check-prefix=ENUM_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_FUNC_DEFAULTPARAM | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_FUNC_BODY | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INSTANCE_SUBSCRIPT_DEFAULTPARAM | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_INSTANCE_SUBSCRIPT_BODY | %FileCheck %s -check-prefix=ENUM_INSTANCE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_SUBSCRIPT_DEFAULTPARAM | %FileCheck %s -check-prefix=ENUM_STATIC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_STATIC_SUBSCRIPT_BODY | %FileCheck %s -check-prefix=ENUM_STATIC


// STRUCT_INSTANCE: Begin completions
// STRUCT_INSTANCE-NOT: init()
// STRUCT_INSTANCE-NOT: staticVar
// STRUCT_INSTANCE-NOT: staticFunc
// STRUCT_INSTANCE-DAG: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#];
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Int#];
// STRUCT_INSTANCE-NOT: init()
// STRUCT_INSTANCE-NOT: staticVar
// STRUCT_INSTANCE-NOT: staticFunc
// STRUCT_INSTANCE: End completions

// STRUCT_STATIC: Begin completions
// STRUCT_STATIC-NOT: init()
// STRUCT_STATIC-NOT: instanceVar 
// STRUCT_STATIC-DAG: Decl[StaticVar]/CurrNominal: staticVar[#Int#];
// STRUCT_STATIC-DAG: Decl[StaticMethod]/CurrNominal: staticFunc()[#Int#];
// STRUCT_STATIC-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(self): MyStruct#})[#() -> Int#];
// STRUCT_STATIC-NOT: init()
// STRUCT_STATIC-NOT: instanceVar 
// STRUCT_STATIC: End completions

struct MyStruct {
  init() { self = MyEnum.east }
  static var staticVar: Int { 1 }
  static func staticFunc() -> Int { 1 }
  var instanceVar: Int { 1 }
  func instanceFunc() -> Int { 1 }

  var testInstanceVarInit: String = (#^STRUCT_INSTANCE_VAR_INIT^#)
  static var testStaticVarInit: String = (#^STRUCT_STATIC_VAR_INIT^#)
  lazy var testLazyVarInit: String = (#^STRUCT_LAZY_VAR_INIT^#)
  var testInstanceVar: String {
    #^STRUCT_INSTANCE_VAR_BODY^#
  }
  static var testStaticVar: String {
    #^STRUCT_STATIC_VAR_BODY^#
  }
  init(testInit: String = #^STRUCT_INITIALIZER_DEFAULTPARAM^#) {
    #^STRUCT_INITIALIZER_BODY^#
  }
  func testInstanceMethod(x: String = #^STRUCT_INSTANCE_FUNC_DEFAULTPARAM^#) {
    #^STRUCT_INSTANCE_FUNC_BODY^#
  }
  static func testStaticMethod(x: String = #^STRUCT_STATIC_FUNC_DEFAULTPARAM^#) {
    #^STRUCT_STATIC_FUNC_BODY^#
  }
  subscript(testInstanceSubscript idx: String = #^STRUCT_INSTANCE_SUBSCRIPT_DEFAULTPARAM^#) {
    #^STRUCT_INSTANCE_SUBSCRIPT_BODY^#
  }
  static subscript(testStaticSubscript idx: String = #^STRUCT_STATIC_SUBSCRIPT_DEFAULTPARAM^#) {
    #^STRUCT_STATIC_SUBSCRIPT_BODY^#
  }
}

// ENUM_INSTANCE: Begin completions
// ENUM_INSTANCE-NOT: east
// ENUM_INSTANCE-NOT: west
// ENUM_INSTANCE-NOT: init()
// ENUM_INSTANCE-NOT: staticVar
// ENUM_INSTANCE-NOT: staticFunc
// ENUM_INSTANCE-DAG: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#];
// ENUM_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Int#];
// ENUM_INSTANCE-NOT: east
// ENUM_INSTANCE-NOT: west
// ENUM_INSTANCE-NOT: init()
// ENUM_INSTANCE-NOT: staticVar
// ENUM_INSTANCE-NOT: staticFunc
// ENUM_INSTANCE: End completions

// ENUM_STATIC: Begin completions
// ENUM_STATIC-NOT: init()
// ENUM_STATIC-NOT: instanceVar 
// ENUM_STATIC-DAG: Decl[EnumElement]/CurrNominal: east[#MyEnum#];
// ENUM_STATIC-DAG: Decl[EnumElement]/CurrNominal: west[#MyEnum#];
// ENUM_STATIC-DAG: Decl[StaticVar]/CurrNominal: staticVar[#Int#];
// ENUM_STATIC-DAG: Decl[StaticMethod]/CurrNominal: staticFunc()[#Int#];
// ENUM_STATIC-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(self): MyEnum#})[#() -> Int#];
// ENUM_STATIC-NOT: init()
// ENUM_STATIC-NOT: instanceVar 
// ENUM_STATIC: End completions

enum MyEnum {
  case east, west
  init() { self = MyEnum.east }
  static var staticVar: Int = 1
  static func staticFunc() -> Int { 1 }
  var instanceVar: Int { 1 }
  func instanceFunc() -> Int { 1 }

  static var testStaticVarInit: String = (#^ENUM_STATIC_VAR_INIT^#)
  var testInstanceVar: String {
    #^ENUM_INSTANCE_VAR_BODY^#
  }
  static var testStaticVar: String {
    #^ENUM_STATIC_VAR_BODY^#
  }
  func init(testInit: String = #^ENUM_INITIALIZER_DEFAULTPARAM^#) {
    #^ENUM_INITIALIZER_BODY^#
  }
  func testInstanceMethod(x: String = #^ENUM_INSTANCE_FUNC_DEFAULTPARAM^#) {
    #^ENUM_INSTANCE_FUNC_BODY^#
  }
  static func testStaticMethod(x: String = #^ENUM_STATIC_FUNC_DEFAULTPARAM^#) {
    #^ENUM_STATIC_FUNC_BODY^#
  }
  subscript(testInstanceSubscript idx: String = #^ENUM_INSTANCE_SUBSCRIPT_DEFAULTPARAM^#) {
    #^ENUM_INSTANCE_SUBSCRIPT_BODY^#
  }
  static subscript(testStaticSubscript idx: String = #^ENUM_STATIC_SUBSCRIPT_DEFAULTPARAM^#) {
    #^ENUM_STATIC_SUBSCRIPT_BODY^#
  }
}
