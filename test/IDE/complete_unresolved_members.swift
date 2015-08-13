// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_1 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_2 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_3 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_4 | FileCheck %s -check-prefix=UNRESOLVED_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_5 | FileCheck %s -check-prefix=UNRESOLVED_1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_6 | FileCheck %s -check-prefix=UNRESOLVED_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_7 | FileCheck %s -check-prefix=UNRESOLVED_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_8 | FileCheck %s -check-prefix=UNRESOLVED_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_9 | FileCheck %s -check-prefix=UNRESOLVED_3

enum SomeEnum1 {
  case South
  case North
}

enum SomeEnum2 {
  case East
  case West
}

struct NotOptions1 {
  static let NotSet = 1;
}

struct SomeOptions1 : OptionSetType {
  let rawValue : Int
  static let Option1 = SomeOptions1(rawValue: 1 << 1)
  static let Option2 = SomeOptions1(rawValue: 1 << 2)
  static let Option3 = SomeOptions1(rawValue: 1 << 3)
  let NotStaticOption = (rawValue: 1 << 4)
}

struct SomeOptions2 : OptionSetType {
  let rawValue : Int
  static let Option4 = SomeOptions2(rawValue: 1 << 1)
  static let Option5 = SomeOptions2(rawValue: 1 << 2)
  static let Option6 = SomeOptions2(rawValue: 1 << 3)
}

func OptionSetTaker1(Op : SomeOptions1) {}

func OptionSetTaker2(Op : SomeOptions2) {}

func EnumTaker1(E : SomeEnum1) {}

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
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrNominal: Option1[#SomeOptions1#]; name=Option1
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrNominal: Option2[#SomeOptions1#]; name=Option2
// UNRESOLVED_1-DAG:  Decl[StaticVar]/CurrNominal: Option3[#SomeOptions1#]; name=Option3
// UNRESOLVED_1-NOT:  SomeOptions2
// UNRESOLVED_1-NOT:  NotOptions1
// UNRESOLVED_1-NOT:  NotStaticOption
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
// UNRESOLVED_2-DAG:  Decl[StaticVar]/CurrNominal: Option4[#SomeOptions2#]; name=Option4
// UNRESOLVED_2-DAG:  Decl[StaticVar]/CurrNominal: Option5[#SomeOptions2#]; name=Option5
// UNRESOLVED_2-DAG:  Decl[StaticVar]/CurrNominal: Option6[#SomeOptions2#]; name=Option6
// UNRESOLVED_2-NOT:  SomeOptions1
// UNRESOLVED_2-NOT:  NotOptions1
// UNRESOLVED_2-NOT:  NotStaticOption
}

class C4 {
  func f1() {
    var E : SomeEnum1
    E = .#^UNRESOLVED_8^#
  }
  func f2() {
    EnumTaker1(.#^UNRESOLVED_9^#)
  }
}
// UNRESOLVED_3: Begin completions
// UNRESOLVED_3-DAG: Decl[EnumElement]/ExprSpecific:     North[#SomeEnum1#]; name=North
// UNRESOLVED_3-DAG: Decl[EnumElement]/ExprSpecific:     South[#SomeEnum1#]; name=South
// UNRESOLVED_3-NOT: SomeEnum2
// UNRESOLVED_3-NOT: SomeOptions1
// UNRESOLVED_3-NOT: SomeOptions2
