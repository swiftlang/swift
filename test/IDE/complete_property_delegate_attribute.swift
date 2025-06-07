// RUN: %batch-code-completion

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

  @MyStruct(arg1: #^ARG_MyEnum_NODOT^#
  var test2
// ARG_MyEnum_NODOT-DAG: Decl[Struct]/CurrModule:            TestStruct[#TestStruct#]; name=TestStruct
// ARG_MyEnum_NODOT-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalMyEnum[#MyEnum#]; name=globalMyEnum

  @MyStruct(arg1: .#^ARG_MyEnum_DOT^#
  var test3
// ARG_MyEnum_DOT: Begin completions, 3 items
// ARG_MyEnum_DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:     east[#MyEnum#]; name=east
// ARG_MyEnum_DOT-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:     west[#MyEnum#]; name=west
// ARG_MyEnum_DOT-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): MyEnum#})[#(into: inout Hasher) -> Void#];

  @MyStruct(arg1: MyEnum.#^ARG_MyEnum_NOBINDING^#)
// ARG_MyEnum_NOBINDING-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: east[#MyEnum#];
// ARG_MyEnum_NOBINDING-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Convertible]: west[#MyEnum#];

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync1() {}

  @MyStruct(arg1: MyEnum.east, #^SECOND_ARG1^#) var test4
// SECOND_ARG1: Begin completions, 1 items
// SECOND_ARG1-DAG: Pattern/Local/Flair[ArgLabels]:     {#arg2: Int#}[#Int#];

  @MyStruct(arg1: MyEnum.east, #^SECOND_ARG1_LABEL_NO_VAR?check=SECOND_ARG1^#)

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync2() {}

  @MyStruct(arg1: MyEnum.east, arg2: #^SECOND_ARG^#) var test4
// SECOND_ARG-DAG: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalInt[#Int#]; name=globalInt

  @MyStruct(arg1: MyEnum.east, arg2: #^SECOND_ARG_NO_VAR?check=SECOND_ARG^#)

  // FIXME: No call patterns are suggested if we are completing in variable with multiple property wrappers (rdar://91480982)
  func sync3() {}

  @MyStruct(#^WITHOUT_VAR?check=AFTER_PAREN^#
}
