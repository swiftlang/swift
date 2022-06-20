// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

enum MyEnum {
  case east, west
}

@propertyWrapper
struct MyStruct {
  var wrappedValue: MyEnum
  init(wrappedValue: MyEnum) {}
  init(arg1: MyEnum, arg2: Int) {}
}

var globalInt: Int = 1
var globalMyEnum: MyEnum = .east

struct TestStruct {
  @MyStruct(#^AFTER_PAREN^#
  var test1
// AFTER_PAREN: Begin completions, 2 items
// AFTER_PAREN-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#wrappedValue: MyEnum#}[')'][#MyStruct#]; name=wrappedValue:
// AFTER_PAREN-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ['(']{#arg1: MyEnum#}, {#arg2: Int#}[')'][#MyStruct#]; name=arg1:arg2:
// AFTER_PAREN: End completions

  @MyStruct(arg1: #^ARG_MyEnum_NODOT^#
  var test2
// ARG_MyEnum_NODOT: Begin completions
// ARG_MyEnum_NODOT-DAG: Decl[Struct]/CurrModule:            TestStruct[#TestStruct#]; name=TestStruct
// ARG_MyEnum_NODOT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalMyEnum[#MyEnum#]; name=globalMyEnum
// ARG_MyEnum_NODOT: End completions

  @MyStruct(arg1: .#^ARG_MyEnum_DOT^#
  var test3
// ARG_MyEnum_DOT: Begin completions, 3 items
// ARG_MyEnum_DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:     east[#MyEnum#]; name=east
// ARG_MyEnum_DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:     west[#MyEnum#]; name=west
// ARG_MyEnum_DOT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MyEnum#})[#(into: inout Hasher) -> Void#];
// ARG_MyEnum_DOT: End completions

  @MyStruct(arg1: MyEnum.#^ARG_MyEnum_NOBINDING^#)
// ARG_MyEnum_NOBINDING: Begin completions
// ARG_MyEnum_NOBINDING-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: east[#MyEnum#];
// ARG_MyEnum_NOBINDING-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: west[#MyEnum#];
// ARG_MyEnum_NOBINDINaG: End completions

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync1() {}

  @MyStruct(arg1: MyEnum.east, #^SECOND_ARG_LABEL^#) var test4
// SECOND_ARG_LABEL: Begin completions, 1 items
// SECOND_ARG_LABEL-DAG: Pattern/Local/Flair[ArgLabels]:     {#arg2: Int#}[#Int#];
// SECOND_ARG_LABEL: End completions

  @MyStruct(arg1: MyEnum.east, #^SECOND_ARG_LABEL_NO_VAR?check=SECOND_ARG_LABEL^#)

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync2() {}

  @MyStruct(arg1: MyEnum.east, arg2: #^SECOND_ARG^#) var test4
// SECOND_ARG: Begin completions
// SECOND_ARG-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalInt[#Int#]; name=globalInt
// SECOND_ARG: End completions

  @MyStruct(arg1: MyEnum.east, arg2: #^SECOND_ARG_NO_VAR?check=SECOND_ARG^#)

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync3() {}

  @MyStruct(#^WITHOUT_VAR?check=AFTER_PAREN^#
}
