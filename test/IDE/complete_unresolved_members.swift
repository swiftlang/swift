// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_1 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_2 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_3 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_4 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_5 | %FileCheck %s -check-prefix=UNRESOLVED_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_6 | %FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_7 | %FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_10 | %FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_11 | %FileCheck %s -check-prefix=UNRESOLVED_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_8 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_9 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_OPT_1 | %FileCheck %s -check-prefix=UNRESOLVED_3_OPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_OPT_2 | %FileCheck %s -check-prefix=UNRESOLVED_3_OPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_OPT_3 | %FileCheck %s -check-prefix=UNRESOLVED_3_OPTOPTOPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_OPT_4 | %FileCheck %s -check-prefix=UNRESOLVED_OPT_4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_12 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_13 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_14 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_15 | %FileCheck %s -check-prefix=UNRESOLVED_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_16 | %FileCheck %s -check-prefix=UNRESOLVED_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_17 | %FileCheck %s -check-prefix=UNRESOLVED_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_18 | %FileCheck %s -check-prefix=UNRESOLVED_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_19 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_20 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_21 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_22 | %FileCheck %s -check-prefix=UNRESOLVED_1_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_22_noreturn | %FileCheck %s -check-prefix=UNRESOLVED_1_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_23 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_24 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_25 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_26 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_27 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_27_NOCRASH > /dev/null
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_28 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_29 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_30 | %FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_31 | %FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_32 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_33 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_34 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_35 | %FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_36 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_37 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_38 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_39 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_40 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_41 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_42 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_43 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN-FIXME: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_44 | %FileCheck %s -check-prefix=UNRESOLVED_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_AVAIL_1 | %FileCheck %s -check-prefix=ENUM_AVAIL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OPTIONS_AVAIL_1 | %FileCheck %s -check-prefix=OPTIONS_AVAIL_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WITH_LITERAL_1 | %FileCheck %s -check-prefix=WITH_LITERAL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WITH_LITERAL_2 | %FileCheck %s -check-prefix=WITH_LITERAL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=WITH_LITERAL_3 | %FileCheck %s -check-prefix=WITH_LITERAL_3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INVALID_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OTHER_FILE_1 %S/Inputs/EnumFromOtherFile.swift | %FileCheck %s -check-prefix=OTHER_FILE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NON_OPT_SET_1 | %FileCheck %s -check-prefix=NON_OPT_SET_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NON_OPT_SET_2 | %FileCheck %s -check-prefix=NON_OPT_SET_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NON_OPT_SET_3 | %FileCheck %s -check-prefix=NON_OPT_SET_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERPOLATION_1 | %FileCheck %s -check-prefix=STRING_INTERPOLATION_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRING_INTERPOLATION_INVALID

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBTYPE_1 | %FileCheck %s -check-prefix=SUBTYPE_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBTYPE_2 | %FileCheck %s -check-prefix=SUBTYPE_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_1 | %FileCheck %s -check-prefix=GENERIC_1 -check-prefix=GENERIC_1_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_2 | %FileCheck %s -check-prefix=GENERIC_1 -check-prefix=GENERIC_1_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_3 | %FileCheck %s -check-prefix=GENERIC_1 -check-prefix=GENERIC_1_U
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_4 | %FileCheck %s -check-prefix=GENERIC_1 -check-prefix=GENERIC_1_INT_NOTIDEAL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STATIC_CLOSURE_1 | %FileCheck %s -check-prefix=STATIC_CLOSURE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_METHOD_1 | %FileCheck %s -check-prefix=OVERLOADED_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_INIT_1 | %FileCheck %s -check-prefix=OVERLOADED_METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERLOADED_INIT_2 | %FileCheck %s -check-prefix=OVERLOADED_METHOD_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_1 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_2 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_3 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_4 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_5 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_6 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_7 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_8 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_9 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_10 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_11 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_12 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_13 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_14 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_15 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_16 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_17 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_18 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_19 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_20 | %FileCheck %s -check-prefix=GENERICPARAM_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERICPARAM_21 | %FileCheck %s -check-prefix=GENERICPARAM_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DECL_MEMBER_INIT_1 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_1 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_2 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_3 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPEPARAM_IN_CONTEXTTYPE_1 | %FileCheck %s -check-prefix=TYPEPARAM_IN_CONTEXTTYPE_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_1 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_2 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_3 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_4 | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_5 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_6 | %FileCheck %s -check-prefix=UNRESOLVED_3_NOTIDEAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TERNARY_CONDITION | %FileCheck %s -check-prefix=TERNARY_CONDITION
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE_OPT | %FileCheck %s -check-prefix=UNRESOLVED_3_OPT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE_FUNCTY | %FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AUTOCLOSURE_FUNCTY_OPT | %FileCheck %s -check-prefix=UNRESOLVED_3_OPT

enum SomeEnum1 {
  case South
  case North
}

enum SomeEnum2 {
  case East
  case West
}

enum SomeEnum3: Hashable {
  case Payload(SomeEnum1)
}

struct NotOptions1 {
  static let NotSet = 1
}

struct SomeOptions1 : OptionSet {
  let rawValue : Int
  static let Option1 = SomeOptions1(rawValue: 1 << 1)
  static let Option2 = SomeOptions1(rawValue: 1 << 2)
  static let Option3 = SomeOptions1(rawValue: 1 << 3)
  let NotStaticOption = SomeOptions1(rawValue: 1 << 4)
  static let NotOption = 1
}

struct SomeOptions2 : OptionSet {
  let rawValue : Int
  static let Option4 = SomeOptions2(rawValue: 1 << 1)
  static let Option5 = SomeOptions2(rawValue: 1 << 2)
  static let Option6 = SomeOptions2(rawValue: 1 << 3)
}

enum EnumAvail1 {
  case aaa
  @available(*, unavailable) case AAA
  @available(*, deprecated) case BBB
}

struct OptionsAvail1 : OptionSet {
  let rawValue: Int
  static let aaa = OptionsAvail1(rawValue: 1 << 0)
  @available(*, unavailable) static let AAA = OptionsAvail1(rawValue: 1 << 0)
  @available(*, deprecated) static let BBB = OptionsAvail1(rawValue: 1 << 1)
}

func OptionSetTaker1(_ Op : SomeOptions1) {}

func OptionSetTaker2(_ Op : SomeOptions2) {}

func OptionSetTaker3(_ Op1: SomeOptions1, _ Op2: SomeOptions2) {}

func OptionSetTaker4(_ Op1: SomeOptions2, _ Op2: SomeOptions1) {}

func OptionSetTaker5(_ Op1: SomeOptions1, _ Op2: SomeOptions2, _ En1 : SomeEnum1, _ En2: SomeEnum2) {}

func OptionSetTaker6(_ Op1: SomeOptions1, _ Op2: SomeOptions2) {}

func OptionSetTaker6(_ Op1: SomeOptions2, _ Op2: SomeOptions1) {}

func OptionSetTaker7(_ Op1: SomeOptions1, _ Op2: SomeOptions2) -> Int {return 0}

func EnumTaker1(_ E : SomeEnum1) {}
func optionalEnumTaker1(_ : SomeEnum1?) {}

class OptionTakerContainer1 {
  func OptionSetTaker1(_ op : SomeOptions1) {}
  func EnumTaker1(_ E : SomeEnum1) {}
}

class C1 {
  func f1() {
    var f : SomeOptions1
    f = .#^UNRESOLVED_1^#
  }
  func f2() {
    var f : SomeOptions1
    f = [.#^UNRESOLVED_2^#
  }
  func f3() {
    var f : SomeOptions1
    f = [.Option1, .#^UNRESOLVED_3^#
  }
}
class C2 {
  func f1() {
    OptionSetTaker1(.#^UNRESOLVED_4^#)
  }
  func f2() {
    OptionSetTaker1([.Option1, .#^UNRESOLVED_5^#])
  }
// UNRESOLVED_1:  Begin completions
// UNRESOLVED_1-NOT:  SomeEnum1
// UNRESOLVED_1-NOT:  SomeEnum2
// UNRESOLVED_1-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option1[#SomeOptions1#]; name=Option1
// UNRESOLVED_1-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option2[#SomeOptions1#]; name=Option2
// UNRESOLVED_1-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option3[#SomeOptions1#]; name=Option3
// UNRESOLVED_1-NOT:  Not

// UNRESOLVED_1_NOTIDEAL:  Begin completions
// UNRESOLVED_1_NOTIDEAL-NOT:  SomeEnum1
// UNRESOLVED_1_NOTIDEAL-NOT:  SomeEnum2
// UNRESOLVED_1_NOTIDEAL-DAG:  Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: Option1[#SomeOptions1#]; name=Option1
// UNRESOLVED_1_NOTIDEAL-DAG:  Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: Option2[#SomeOptions1#]; name=Option2
// UNRESOLVED_1_NOTIDEAL-DAG:  Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: Option3[#SomeOptions1#]; name=Option3
// UNRESOLVED_1_NOTIDEAL-NOT:  Not
}

class C3 {
  func f1() {
    OptionSetTaker2(.#^UNRESOLVED_6^#)
  }
  func f2() {
    OptionSetTaker2([.Option4, .#^UNRESOLVED_7^#])
  }
// UNRESOLVED_2:  Begin completions
// UNRESOLVED_2-NOT:  SomeEnum1
// UNRESOLVED_2-NOT:  SomeEnum2
// UNRESOLVED_2-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option4[#SomeOptions2#]; name=Option4
// UNRESOLVED_2-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option5[#SomeOptions2#]; name=Option5
// UNRESOLVED_2-DAG:  Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option6[#SomeOptions2#]; name=Option6
// UNRESOLVED_2-NOT:  Not
}

class C4 {
  func f1() {
    var E : SomeEnum1
    E = .#^UNRESOLVED_8^#
  }
  func f2() {
    EnumTaker1(.#^UNRESOLVED_9^#)
  }
  func f3() {
    OptionSetTaker5(.Option1, .Option4, .#^UNRESOLVED_12^#, .West)
  }
  func f4() {
    var _: SomeEnum1? = .#^UNRESOLVED_OPT_1^#
  }
  func f5() {
    optionalEnumTaker1(.#^UNRESOLVED_OPT_2^#)
  }
  func f6() {
    var _: SomeEnum1??? = .#^UNRESOLVED_OPT_3^#
  }
}
// UNRESOLVED_3: Begin completions, 2 items
// UNRESOLVED_3-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: North[#SomeEnum1#]; name=North
// UNRESOLVED_3-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: South[#SomeEnum1#]; name=South
// UNRESOLVED_3-NOT: SomeOptions1
// UNRESOLVED_3-NOT: SomeOptions2
// UNRESOLVED_3-NOT: none
// UNRESOLVED_3-NOT: some(

// UNRESOLVED_3_NOTIDEAL: Begin completions, 2 items
// UNRESOLVED_3_NOTIDEAL-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: North[#SomeEnum1#]; name=North
// UNRESOLVED_3_NOTIDEAL-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: South[#SomeEnum1#]; name=South
// UNRESOLVED_3_NOTIDEAL-NOT: SomeOptions1
// UNRESOLVED_3_NOTIDEAL-NOT: SomeOptions2
// UNRESOLVED_3_NOTIDEAL-NOT: none
// UNRESOLVED_3_NOTIDEAL-NOT: some(

// UNRESOLVED_3_OPT: Begin completions, 5 items
// UNRESOLVED_3_OPT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]: North[#SomeEnum1#];
// UNRESOLVED_3_OPT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]: South[#SomeEnum1#];
// UNRESOLVED_3_OPT-DAG: Keyword[nil]/None/Erase[1]: nil[#SomeEnum1?#]; name=nil
// UNRESOLVED_3_OPT-DAG: Decl[EnumElement]/CurrNominal/IsSystem: none[#Optional<SomeEnum1>#]; name=none
// UNRESOLVED_3_OPT-DAG: Decl[EnumElement]/CurrNominal/IsSystem: some({#SomeEnum1#})[#Optional<SomeEnum1>#];
// UNRESOLVED_3_OPT-NOT: init({#(some):
// UNRESOLVED_3_OPT-NOT: init({#nilLiteral:

// UNRESOLVED_3_OPTOPTOPT: Begin completions, 5 items
// UNRESOLVED_3_OPTOPTOPT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]: North[#SomeEnum1#];
// UNRESOLVED_3_OPTOPTOPT-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]: South[#SomeEnum1#];
// UNRESOLVED_3_OPTOPTOPT-DAG: Keyword[nil]/None/Erase[1]: nil[#SomeEnum1???#]; name=nil
// UNRESOLVED_3_OPTOPTOPT-DAG: Decl[EnumElement]/CurrNominal/IsSystem: none[#Optional<SomeEnum1??>#]; name=none
// UNRESOLVED_3_OPTOPTOPT-DAG: Decl[EnumElement]/CurrNominal/IsSystem: some({#SomeEnum1??#})[#Optional<SomeEnum1??>#];
// UNRESOLVED_3_OPTOPTOPT-NOT: init({#(some):
// UNRESOLVED_3_OPTOPTOPT-NOT: init({#nilLiteral:

enum Somewhere {
  case earth, mars
}
extension Optional where Wrapped == Somewhere {
  init(str: String) { fatalError() }
  static var nowhere: Self { return nil }
}
func testOptionalWithCustomExtension() {
  var _: Somewhere? = .#^UNRESOLVED_OPT_4^#
// UNRESOLVED_OPT_4: Begin completions, 7 items
// UNRESOLVED_OPT_4-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]:     earth[#Somewhere#];
// UNRESOLVED_OPT_4-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Convertible]:     mars[#Somewhere#];
// UNRESOLVED_OPT_4-DAG: Keyword[nil]/None/Erase[1]: nil[#Somewhere?#]; name=nil
// UNRESOLVED_OPT_4-DAG: Decl[EnumElement]/CurrNominal/IsSystem: none[#Optional<Somewhere>#]; name=none
// UNRESOLVED_OPT_4-DAG: Decl[EnumElement]/CurrNominal/IsSystem: some({#Somewhere#})[#Optional<Somewhere>#];
// UNRESOLVED_OPT_4-DAG: Decl[Constructor]/CurrNominal: init({#str: String#})[#Optional<Somewhere>#]; name=init(str: String)
// UNRESOLVED_OPT_4-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Identical]: nowhere[#Optional<Somewhere>#]; name=nowhere
// UNRESOLVED_OPT_4-NOT: init({#(some):
// UNRESOLVED_OPT_4-NOT: init({#nilLiteral:
// UNRESOLVED_OPT_4: End completions
}


class C5 {
  func f1() {
    OptionSetTaker3(.Option1, .#^UNRESOLVED_10^#)
  }
  func f2() {
    OptionSetTaker4(.#^UNRESOLVED_11^#, .Option1)
  }
}

OptionSetTaker5(.Option1, .Option4, .#^UNRESOLVED_13^#, .West)
OptionSetTaker5(.#^UNRESOLVED_14^#, .Option4, .South, .West)
OptionSetTaker5([.#^UNRESOLVED_15^#], .Option4, .South, .West)

OptionSetTaker6(.#^UNRESOLVED_16^#, .Option4)
OptionSetTaker6(.Option4, .#^UNRESOLVED_17^#,)

var a = {() in
  OptionSetTaker5([.#^UNRESOLVED_18^#], .Option4, .South, .West)
}
var Container = OptionTakerContainer1()
Container.OptionSetTaker1(.#^UNRESOLVED_19^#)
Container.EnumTaker1(.#^UNRESOLVED_20^#

func parserSync() {}

// UNRESOLVED_4: Begin completions
// UNRESOLVED_4-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option1[#SomeOptions1#]; name=Option1
// UNRESOLVED_4-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option2[#SomeOptions1#]; name=Option2
// UNRESOLVED_4-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: Option3[#SomeOptions1#]; name=Option3
// UNRESOLVED_4-NOT: Option4
// UNRESOLVED_4-NOT: Option5
// UNRESOLVED_4-NOT: Option6

var OpIns1 : SomeOptions1 = .#^UNRESOLVED_21^#

var c1 = {() -> SomeOptions1 in
  return .#^UNRESOLVED_22^#
}

var c1_noreturn = {() -> SomeOptions1 in
  .#^UNRESOLVED_22_noreturn^#
}

class C6 {
  func f1() -> SomeOptions1 {
    return .#^UNRESOLVED_23^#
  }
  func f2(p : SomeEnum3) {
  switch p {
  case .Payload(.#^UNRESOLVED_24^#)
  }
  }
}

class C6 {
  func f1(e: SomeEnum1) {
    if let x = Optional(e) where x == .#^UNRESOLVED_25^#
  }
}
class C7 {}
extension C7 {
  func extendedf1(_ e :SomeEnum1) {}
}

var cInst1 = C7()
cInst1.extendedf1(.#^UNRESOLVED_26^#

func nocrash1() -> SomeEnum1 {
  return .#^UNRESOLVED_27_NOCRASH^#
}

func resetParser1() {}

func f() -> SomeEnum1 {
  return .#^UNRESOLVED_27^#
}

let TopLevelVar1 = OptionSetTaker7([.#^UNRESOLVED_28^#], [.Option4])

let TopLevelVar2 = OptionSetTaker1([.#^UNRESOLVED_29^#])

let TopLevelVar3 = OptionSetTaker7([.Option1], [.#^UNRESOLVED_30^#])
let TopLevelVar4 = OptionSetTaker7([.Option1], [.Option4, .#^UNRESOLVED_31^#])

let _: [SomeEnum1] = [.#^UNRESOLVED_32^#]
let _: [SomeEnum1] = [.South, .#^UNRESOLVED_33^#]
let _: [SomeEnum1] = [.South, .#^UNRESOLVED_34^# .South]

let _: [SomeEnum1:SomeOptions1] = [.South:.Option1, .South:.#^UNRESOLVED_35^#]
let _: [SomeEnum1:SomeOptions1] = [.South:.Option1, .#^UNRESOLVED_36^#:.Option1]
let _: [SomeEnum1:SomeOptions1] = [.South:.Option1, .#^UNRESOLVED_37^#]
let _: [SomeEnum1:SomeOptions1] = [.South:.Option1, .#^UNRESOLVED_38^#:]
let _: [SomeEnum1:SomeOptions1] = [.#^UNRESOLVED_39^#]
let _: [SomeEnum1:SomeOptions1] = [.#^UNRESOLVED_40^#:]
let _: [SomeEnum1:SomeEnum3] = [.South:.Payload(.South), .South: .Payload(.#^UNRESOLVED_41^#)]
let _: [SomeEnum3:SomeEnum1] = [.Payload(.South):.South, .Payload(.#^UNRESOLVED_42^#):.South]
let _: [SomeEnum3:SomeEnum1] = [.Payload(.South):.South, .Payload(.#^UNRESOLVED_43^#):]
let _: [SomeEnum3:SomeEnum1] = [.Payload(.South):.South, .Payload(.#^UNRESOLVED_44^#)]

func testAvail1(_ x: EnumAvail1) {
  testAvail1(.#^ENUM_AVAIL_1^#)
}
// ENUM_AVAIL_1: Begin completions, 2 items
// ENUM_AVAIL_1-NOT: AAA
// ENUM_AVAIL_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: aaa[#EnumAvail1#];
// ENUM_AVAIL_1-DAG: Decl[EnumElement]/ExprSpecific/NotRecommended/TypeRelation[Identical]: BBB[#EnumAvail1#];
// ENUM_AVAIL_1-NOT: AAA
// ENUM_AVAIL_1: End completions

func testAvail2(_ x: OptionsAvail1) {
  testAvail2(.#^OPTIONS_AVAIL_1^#)
}
// OPTIONS_AVAIL_1: Begin completions
// ENUM_AVAIL_1-NOT: AAA
// OPTIONS_AVAIL_1-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]: aaa[#OptionsAvail1#];
// OPTIONS_AVAIL_1-DAG: Decl[StaticVar]/ExprSpecific/NotRecommended/TypeRelation[Identical]: BBB[#OptionsAvail1#];
// OPTIONS_AVAIL_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init({#rawValue: Int#})[#OptionsAvail1#]
// ENUM_AVAIL_1-NOT: AAA
// OPTIONS_AVAIL_1: End completions

func testWithLiteral1() {
  struct S {
    enum MyEnum { case myCase }
    enum Thing { case thingCase }
    var thing: Thing
    func takeEnum(thing: MyEnum, other: Double) {}
  }
  let s: S
  _ = s.takeEnum(thing: .#^WITH_LITERAL_1^#, other: 1.0)
// WITH_LITERAL_1: Begin completions, 1 items
// WITH_LITERAL_1-NEXT: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: myCase[#S.MyEnum#];
// WITH_LITERAL_1-NEXT: End completions
}
func testWithLiteral2() {
  struct S {
    enum MyEnum { case myCase }
    enum Thing { case thingCase }
    var thing: Thing
    func takeEnum(thing: MyEnum, other: Int) {}
    func takeEnum(thing: MyEnum, other: Double) {}
  }
  let s: S
  _ = s.takeEnum(thing: .#^WITH_LITERAL_2^#, other: 1.0)
}
func testWithLiteral3() {
  struct S {
    enum MyEnum { case myCase }
    enum Thing { case thingCase }
    var thing: Thing
    func takeEnum(thing: MyEnum, other: Int) {}
    func takeEnum(thing: MyEnum, other: Double) {}
    func test(s: S) {
      _ = s.takeEnum(thing: .#^WITH_LITERAL_3^#, other: 1.0)
// WITH_LITERAL_3: Begin completions, 1 items
// WITH_LITERAL_3-NEXT: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: myCase[#MyEnum#];
// WITH_LITERAL_3-NEXT: End completions
    }
  }
}

func testInvalid1() {
  func invalid() -> NoSuchEnum {
    return .#^INVALID_1^# // Don't crash.
  }
}

func enumFromOtherFile() -> EnumFromOtherFile {
  return .#^OTHER_FILE_1^# // Don't crash.
}
// OTHER_FILE_1: Begin completions
// OTHER_FILE_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: b({#String#})[#EnumFromOtherFile#];
// OTHER_FILE_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: a({#Int#})[#EnumFromOtherFile#];
// OTHER_FILE_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: c[#EnumFromOtherFile#];
// OTHER_FILE_1: End completions

struct NonOptSet {
  static let a = NonOptSet()
  static let wrongType = 1
  let notStatic = NonOptSet()
  init(x: Int, y: Int) {}
  init() {}
  static func b() -> NonOptSet { return NonOptSet() }
  static func wrongType() -> Int { return 0 }
  func notStatic() -> NonOptSet { return NonOptSet() }
}

func testNonOptSet() {
  let x: NonOptSet
  x = .#^NON_OPT_SET_1^#
}
// NON_OPT_SET_1: Begin completions, 4 items
// NON_OPT_SET_1-DAG: Decl[StaticVar]/ExprSpecific/TypeRelation[Identical]:    a[#NonOptSet#]
// NON_OPT_SET_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]:  init({#x: Int#}, {#y: Int#})[#NonOptSet#]
// NON_OPT_SET_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]:  init()[#NonOptSet#]
// NON_OPT_SET_1-DAG: Decl[StaticMethod]/ExprSpecific/TypeRelation[Identical]: b()[#NonOptSet#]
// NON_OPT_SET_1: End completions

func testNonOptSet() {
  let x: NonOptSet = .#^NON_OPT_SET_2^#
}

func testNonOptSet() -> NonOptSet {
  return .#^NON_OPT_SET_3^#
}

func testInStringInterpolation() {
  enum MyEnum { case foo, bar }
  func takeEnum(_ e: MyEnum) -> MyEnum { return e }
  let x = "enum: \(takeEnum(.#^STRING_INTERPOLATION_1^#))"
  let y = "enum: \(.#^STRING_INTERPOLATION_INVALID^#)" // Dont'crash.
}
// STRING_INTERPOLATION_1: Begin completions
// STRING_INTERPOLATION_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: foo[#MyEnum#];
// STRING_INTERPOLATION_1-DAG: Decl[EnumElement]/ExprSpecific/TypeRelation[Identical]: bar[#MyEnum#];
// STRING_INTERPOLATION_1: End completions

class BaseClass {
  class SubClass : BaseClass { init() {} }
  static var subInstance: SubClass = SubClass()
  init() {}
  init?(failable: Void) {}
}
protocol MyProtocol {
  typealias Concrete1 = BaseClass
  typealias Concrete2 = AnotherTy
}
extension BaseClass : MyProtocol {}
struct AnotherTy: MyProtocol {}
func testSubType() {
  var _: BaseClass = .#^SUBTYPE_1^#
}
// SUBTYPE_1: Begin completions, 3 items
// SUBTYPE_1-NOT: init(failable:
// SUBTYPE_1-NOT: Concrete1(
// SUBTYPE_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init()[#BaseClass#];
// SUBTYPE_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: SubClass()[#BaseClass.SubClass#];
// SUBTYPE_1-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: subInstance[#BaseClass.SubClass#];
// SUBTYPE_1: End completions

func testMemberTypealias() {
  var _: MyProtocol = .#^SUBTYPE_2^#
}
// SUBTYPE_2-NOT: Begin completions

enum Generic<T> {
  case contains(content: T)
  case empty
  static func create(_: T) -> Generic<T> { fatalError() }
}
func takeGenericInt(_: Generic<Int>) { }
func takeGenericU<U>(_: Generic<U>) { }
func testGeneric() {
  do {
    let _: Generic<Int> = .#^GENERIC_1^#
  }
  takeGenericInt(.#^GENERIC_2^#)
  takeGenericU(.#^GENERIC_3^#)
}

switch Generic<Int>.empty {
case let .#^GENERIC_4^#
}
// GENERIC_1_INT: Begin completions
// GENERIC_1_INT-DAG: Decl[EnumElement]/ExprSpecific: contains({#content: Int#})[#Generic<Int>#];
// GENERIC_1_INT-DAG: Decl[EnumElement]/ExprSpecific: empty[#Generic<Int>#];
// GENERIC_1_INT-DAG: Decl[StaticMethod]/ExprSpecific: create({#Int#})[#Generic<Int>#];
// GENERIC_1_INT: End completions

// GENERIC_1_INT_NOTIDEAL: Begin completions
// GENERIC_1_INT_NOTIDEAL-DAG: Decl[EnumElement]/CurrNominal: contains({#content: Int#})[#Generic<Int>#];
// GENERIC_1_INT_NOTIDEAL-DAG: Decl[EnumElement]/CurrNominal: empty[#Generic<Int>#];
// GENERIC_1_INT_NOTIDEAL-DAG: Decl[StaticMethod]/CurrNominal: create({#Int#})[#Generic<Int>#];
// GENERIC_1_INT_NOTIDEAL: End completions

// GENERIC_1_U: Begin completions
// GENERIC_1_U-DAG: Decl[EnumElement]/CurrNominal: contains({#content: U#})[#Generic<U>#];
// GENERIC_1_U-DAG: Decl[EnumElement]/CurrNominal: empty[#Generic<U>#];
// GENERIC_1_U-DAG: Decl[StaticMethod]/CurrNominal: create({#U#})[#Generic<U>#];
// GENERIC_1_U: End completions

struct HasCreator {
  static var create: () -> HasCreator = { fatalError() }
  static var create_curried: () -> () -> HasCreator = { fatalError() }
}
func testHasStaticClosure() {
  let _: HasCreator = .#^STATIC_CLOSURE_1^#
}
// STATIC_CLOSURE_1: Begin completions, 2 items
// STATIC_CLOSURE_1-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Identical]: init()[#HasCreator#];
// FIXME: Suggest 'create()[#HasCreateor#]', not 'create'.
// STATIC_CLOSURE_1-DAG: Decl[StaticVar]/CurrNominal:        create[#() -> HasCreator#];
// STATIC_CLOSURE_1-NOT: create_curried
// STATIC_CLOSURE_1: End completions

struct HasOverloaded {
  init(e: SomeEnum1) {}
  init(e: SomeEnum2) {}
  func takeEnum(_ e: SomeEnum1) -> Int { return 0 }
  func takeEnum(_ e: SomeEnum2) -> Int { return 0 }
}
func testOverload(val: HasOverloaded) {
  let _ = val.takeEnum(.#^OVERLOADED_METHOD_1^#)
// OVERLOADED_METHOD_1: Begin completions, 4 items
// OVERLOADED_METHOD_1-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: South[#SomeEnum1#]; name=South
// OVERLOADED_METHOD_1-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: North[#SomeEnum1#]; name=North
// OVERLOADED_METHOD_1-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: East[#SomeEnum2#]; name=East
// OVERLOADED_METHOD_1-DAG: Decl[EnumElement]/CurrNominal/TypeRelation[Identical]: West[#SomeEnum2#]; name=West
// OVERLOADED_METHOD_1: End completions

  let _ = HasOverloaded.init(e: .#^OVERLOADED_INIT_1^#)
// Same as OVERLOADED_METHOD_1.

  let _ = HasOverloaded(e: .#^OVERLOADED_INIT_2^#)
// Same as OVERLOADED_METHOD_1.
}

protocol HasStatic {
  static var instance: Self { get }
}
func receiveHasStatic<T: HasStatic>(x: T)  {}
func testingGenericParam1<T: HasStatic>(x: inout T, fn: (T) -> Void) -> T {
  x = .#^GENERICPARAM_1^#
// GENERICPARAM_1: Begin completions, 1 items
// GENERICPARAM_1: Decl[StaticVar]/CurrNominal: instance[#HasStatic#]; name=instance
// GENERICPARAM_1: End completions

  /* Parser sync. */;

  let _: (Int, T) = (1, .#^GENERICPARAM_2^#)
  // Same as GENERICPARAM_1.

  (_, x) = (1, .#^GENERICPARAM_3^#)
  // Same as GENERICPARAM_1.

  let _ = fn(.#^GENERICPARAM_4^#)
  // Same as GENERICPARAM_1.

  let _ = receiveHasStatic(x: .#^GENERICPARAM_5^#)
  // Same as GENERICPARAM_1.

  let _ = { () -> T in
    return .#^GENERICPARAM_6^#
    // Same as GENERICPARAM_1.
  }
  let _: () -> T = {
    return .#^GENERICPARAM_7^#
    // Same as GENERICPARAM_1.
  }
  let _ = { (_: InvalidTy) -> T in
    return .#^GENERICPARAM_8^#
    // Same as GENERICPARAM_1.
  }

  if case .#^GENERICPARAM_9^# = x {}
  // Same as GENERICPARAM_1.

  return .#^GENERICPARAM_10^#
  // Same as GENERICPARAM_1.
}

class C<T: HasStatic> {

  var t: T = .instance

  func foo(x: T) -> T {
    return .#^GENERICPARAM_11^#
    // Same as GENERICPARAM_1.
  }
  func bar<U: HasStatic>(x: U) -> U {
    return .#^GENERICPARAM_12^#
    // Same as GENERICPARAM_1.
  }

  func testing() {
    let _ = foo(x: .#^GENERICPARAM_13^#)
    // Same as GENERICPARAM_1.
    let _ = bar(x: .#^GENERICPARAM_14^#)
    // Same as GENERICPARAM_1.

    t = .#^GENERICPARAM_15^#
    // Same as GENERICPARAM_1.

    /* Parser sync. */; func sync1() {}
    self.t = .#^GENERICPARAM_16^#
    // Same as GENERICPARAM_1.

    /* Parser sync. */; func sync2() {}
    (_, t) = (1, .#^GENERICPARAM_17^#)
    // Same as GENERICPARAM_1.

    (_, self.t) = (1, .#^GENERICPARAM_18^#)
    // Same as GENERICPARAM_1.
  }
}

func testingGenericParam2<X>(obj: C<X>) {
  let _ = obj.foo(x: .#^GENERICPARAM_19^#)
  // Same as GENERICPARAM_1.
  let _ = obj.bar(x: .#^GENERICPARAM_20^#)
  // Same as GENERICPARAM_1.
  obj.t = .#^GENERICPARAM_21^#
  // Same as GENERICPARAM_1.
}

struct TestingStruct {
  var value: SomeEnum1 = .#^DECL_MEMBER_INIT_1^#
}

func testDefaultArgument(arg: SomeEnum1 = .#^DEFAULT_ARG_1^#) {}
class TestDefalutArg {
  func method(arg: SomeEnum1 = .#^DEFAULT_ARG_2^#) {}
  init(arg: SomeEnum1 = .#^DEFAULT_ARG_3^#) {}
}


struct ConcreteMyProtocol: MyProtocol {}
struct OtherProtocol {}
struct ConcreteOtherProtocol: OtherProtocol {}

struct MyStruct<T> {}
extension MyStruct where T: MyProtocol {
  static var myProtocolOption: MyStruct<ConcreteMyProtocol> { fatalError() }
}
extension MyStruct where T: OtherProtocol {
  static var otherProtocolOption: MyStruct<ConcreteOtherProtocol> { fatalError() }
}

func receiveMyStructOfMyProtocol<T: MyProtocol>(value: MyStruct<T>) {}
func testTypeParamInContextType() {
  receiveMyStructOfMyProtocol(value: .#^TYPEPARAM_IN_CONTEXTTYPE_1^#)
// TYPEPARAM_IN_CONTEXTTYPE_1: Begin completions, 2 items
// TYPEPARAM_IN_CONTEXTTYPE_1-NOT: otherProtocolOption
// TYPEPARAM_IN_CONTEXTTYPE_1-DAG: Decl[Constructor]/CurrNominal:      init()[#MyStruct<MyProtocol>#];
// TYPEPARAM_IN_CONTEXTTYPE_1-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: myProtocolOption[#MyStruct<ConcreteMyProtocol>#];
// TYPEPARAM_IN_CONTEXTTYPE_1: End completions
}

func testTernaryOperator(cond: Bool) {
  let _: SomeEnum1 = cond ? .#^TERNARY_1^#
  func sync(){}
  let _: SomeEnum1 = cond ? .#^TERNARY_2^# :
  func sync(){}
  let _: SomeEnum1 = cond ? .#^TERNARY_3^# : .South
  func sync(){}
  let _: SomeEnum1 = cond ? .South : .#^TERNARY_4^#
}

func testTernaryOperator2(cond: Bool) {
  let _: SomeEnum1 = cond ? .#^TERNARY_5^# : .bogus
  func sync(){}
  let _: SomeEnum1 = cond ? .bogus : .#^TERNARY_6^#
  func sync(){}
  let _: SomeEnum1 = .#^TERNARY_CONDITION^# ? .bogus : .bogus
// TERNARY_CONDITION: Begin completions
// TERNARY_CONDITION-DAG: Decl[Constructor]/CurrNominal/IsSystem/TypeRelation[Identical]: init()[#Bool#]; name=init()
// TERNARY_CONDITION: End completions
}

func receiveAutoclosure(_: @autoclosure () -> SomeEnum1) {}
func receiveAutoclosureOpt(_: @autoclosure () -> SomeEnum1?) {}
func testAutoclosre() {
  receiveAutoclosure(.#^AUTOCLOSURE^#)
  // Same as UNRESOLVED_3

  receiveAutoclosureOpt(.#^AUTOCLOSURE_OPT^#)
  // Same as UNRESOLVED_3_OPT
}
func testAutoclosreFuncTy(fn: (@autoclosure () -> SomeEnum1) -> Void, fnOpt: (@autoclosure () -> SomeEnum1?) -> Void) {
  fn(.#^AUTOCLOSURE_FUNCTY^#)
  // Same as UNRESOLVED_3
  fnOpt(.#^AUTOCLOSURE_FUNCTY_OPT^#)
  // Same as UNRESOLVED_3_OPT
}
