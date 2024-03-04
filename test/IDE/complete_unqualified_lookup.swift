// RUN: %batch-code-completion

// STRUCT_INSTANCE-NOT: init()
// STRUCT_INSTANCE-NOT: staticVar
// STRUCT_INSTANCE-NOT: staticFunc
// STRUCT_INSTANCE-DAG: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#];
// STRUCT_INSTANCE-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc()[#Int#];
// STRUCT_INSTANCE-NOT: init()
// STRUCT_INSTANCE-NOT: staticVar
// STRUCT_INSTANCE-NOT: staticFunc

// STRUCT_STATIC-NOT: init()
// STRUCT_STATIC-NOT: instanceVar 
// STRUCT_STATIC-DAG: Decl[StaticVar]/CurrNominal: staticVar[#Int#];
// STRUCT_STATIC-DAG: Decl[StaticMethod]/CurrNominal: staticFunc()[#Int#];
// STRUCT_STATIC-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(self): MyStruct#})[#() -> Int#];
// STRUCT_STATIC-NOT: init()
// STRUCT_STATIC-NOT: instanceVar 

struct MyStruct {
  init() { self = MyEnum.east }
  static var staticVar: Int { 1 }
  static func staticFunc() -> Int { 1 }
  var instanceVar: Int { 1 }
  func instanceFunc() -> Int { 1 }

  var testInstanceVarInit: String = (#^STRUCT_INSTANCE_VAR_INIT?check=STRUCT_STATIC^#)
  static var testStaticVarInit: String = (#^STRUCT_STATIC_VAR_INIT?check=STRUCT_STATIC^#)
  lazy var testLazyVarInit: String = (#^STRUCT_LAZY_VAR_INIT?check=STRUCT_INSTANCE^#)
  var testInstanceVar: String {
    #^STRUCT_INSTANCE_VAR_BODY?check=STRUCT_INSTANCE^#
  }
  static var testStaticVar: String {
    #^STRUCT_STATIC_VAR_BODY?check=STRUCT_STATIC^#
  }
  init(testInit: String = #^STRUCT_INITIALIZER_DEFAULTPARAM?check=STRUCT_STATIC^#) {
    #^STRUCT_INITIALIZER_BODY?check=STRUCT_INSTANCE^#
  }
  func testInstanceMethod(x: String = #^STRUCT_INSTANCE_FUNC_DEFAULTPARAM?check=STRUCT_STATIC^#) {
    #^STRUCT_INSTANCE_FUNC_BODY?check=STRUCT_INSTANCE^#
  }
  static func testStaticMethod(x: String = #^STRUCT_STATIC_FUNC_DEFAULTPARAM?check=STRUCT_STATIC^#) {
    #^STRUCT_STATIC_FUNC_BODY?check=STRUCT_STATIC^#
  }
  subscript(testInstanceSubscript idx: String = #^STRUCT_INSTANCE_SUBSCRIPT_DEFAULTPARAM?check=STRUCT_STATIC^#) {
    #^STRUCT_INSTANCE_SUBSCRIPT_BODY?check=STRUCT_INSTANCE^#
  }
  static subscript(testStaticSubscript idx: String = #^STRUCT_STATIC_SUBSCRIPT_DEFAULTPARAM?check=STRUCT_STATIC^#) {
    #^STRUCT_STATIC_SUBSCRIPT_BODY?check=STRUCT_STATIC^#
  }
}

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

// ENUM_STATIC-NOT: init()
// ENUM_STATIC-NOT: instanceVar 
// ENUM_STATIC-DAG: Decl[EnumElement]/CurrNominal: east[#MyEnum#];
// ENUM_STATIC-DAG: Decl[EnumElement]/CurrNominal: west[#MyEnum#];
// ENUM_STATIC-DAG: Decl[StaticVar]/CurrNominal: staticVar[#Int#];
// ENUM_STATIC-DAG: Decl[StaticMethod]/CurrNominal: staticFunc()[#Int#];
// ENUM_STATIC-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc({#(self): MyEnum#})[#() -> Int#];
// ENUM_STATIC-NOT: init()
// ENUM_STATIC-NOT: instanceVar 

enum MyEnum {
  case east, west
  init() { self = MyEnum.east }
  static var staticVar: Int = 1
  static func staticFunc() -> Int { 1 }
  var instanceVar: Int { 1 }
  func instanceFunc() -> Int { 1 }

  static var testStaticVarInit: String = (#^ENUM_STATIC_VAR_INIT?check=ENUM_STATIC^#)
  var testInstanceVar: String {
    #^ENUM_INSTANCE_VAR_BODY?check=ENUM_INSTANCE^#
  }
  static var testStaticVar: String {
    #^ENUM_STATIC_VAR_BODY?check=ENUM_STATIC^#
  }
  init(testInit: String = #^ENUM_INITIALIZER_DEFAULTPARAM?check=ENUM_STATIC^#) {
    #^ENUM_INITIALIZER_BODY?check=ENUM_INSTANCE^#
  }
  func testInstanceMethod(x: String = #^ENUM_INSTANCE_FUNC_DEFAULTPARAM?check=ENUM_STATIC^#) {
    #^ENUM_INSTANCE_FUNC_BODY?check=ENUM_INSTANCE^#
  }
  static func testStaticMethod(x: String = #^ENUM_STATIC_FUNC_DEFAULTPARAM?check=ENUM_STATIC^#) {
    #^ENUM_STATIC_FUNC_BODY?check=ENUM_STATIC^#
  }
  subscript(testInstanceSubscript idx: String = #^ENUM_INSTANCE_SUBSCRIPT_DEFAULTPARAM?check=ENUM_STATIC^#) {
    #^ENUM_INSTANCE_SUBSCRIPT_BODY?check=ENUM_INSTANCE^#
  }
  static subscript(testStaticSubscript idx: String = #^ENUM_STATIC_SUBSCRIPT_DEFAULTPARAM?check=ENUM_STATIC^#) {
    #^ENUM_STATIC_SUBSCRIPT_BODY?check=ENUM_STATIC^#
  }
}

// rdar://123790296
func testBindingFromDefer(_ x: Int?) {
  guard let bar = x else { return }
  defer {
    #^BINDING_FROM_DEFER^#
    // BINDING_FROM_DEFER-DAG: Decl[LocalVar]/Local: x[#Int?#]; name=x
    // BINDING_FROM_DEFER-DAG: Decl[LocalVar]/Local: bar[#Int#]; name=bar
  }
}
